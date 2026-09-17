{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}

{- HLINT ignore "Use newtype instead of data" -}

module Main where

import Control.Monad
import Data.Default
import Data.Foldable
import Data.List ((!?))
import Data.Text qualified as T
import Data.Traversable
import Mischief.ECS.Interval qualified as Interval
import Mischief.ECS.Observers qualified as Observers
import Mischief.ECS.Prelude
import Mischief.ECS.Stdin qualified as Stdin
import Mischief.ECS.Stdout
import Mischief.ECS.Time qualified as Time
import Mischief.ECS.Timer (Timer)
import Mischief.ECS.Timer qualified as Timer
import Mischief.ECS.World.Query
import System.Random
import System.Random.Stateful

main :: IO ()
main = do
  app <- newApp
  addPlugin @MainPlugin app
  runApp app

data MainPlugin

instance Plugin MainPlugin where
  init = do
    Stdin.init

    systems (spawnGrid, spawnWalls)
      & schedule Startup

    systems printGrid
      & schedule Update

    insertRes =<< newGen

  deps = [dep @PlayerPlugin, dep @EnemyPlugin, dep @TimePlugin]

data PlayerPlugin

instance Plugin PlayerPlugin where
  init = do
    systems spawnPlayer
      & after spawnGrid
      & schedule Startup

    systems movePlayer
      & schedule Update

data EnemyPlugin

instance Plugin EnemyPlugin where
  init = do
    systems spawnEnemies
      & schedule Startup

    systems moveEnemies
      & schedule Update

data Tile = Tile deriving (Component)

data Pos = Pos (Int, Int) deriving (Component)

data Grid = Grid [[Entity]] deriving (Component)

gridH :: Int
gridH = 10

gridW :: Int
gridW = 20

spawnGrid :: System ()
spawnGrid = do
  tiles <-
    for [0 .. gridH - 1] $ \i ->
      for [0 .. gridW - 1] $ \j ->
        spawn (Tile, Pos (i, j))

  insertRes $ Grid tiles

getTile :: (Int, Int) -> Grid -> Maybe Entity
getTile (x, y) (Grid tiles) = do
  line <- tiles !? x
  line !? y

moveBy :: (Int, Int) -> Pos -> Grid -> Maybe Entity
moveBy (x, y) (Pos (x', y')) = getTile (x' + x, y' + y)

data Player = Player deriving (Component)

data OnTile = OnTile

instance Component OnTile where
  type IsExclusiveRel OnTile = True

spawnPlayer :: System ()
spawnPlayer = do
  Just grid <- res @Grid
  for_ (getTile (5, 5) grid) $ \tile ->
    void $ spawn (Player, Rel {comp = OnTile, target = tile})

data Wall = Wall deriving (Component)

spawnWall :: (Int, Int) -> System (Maybe Entity)
spawnWall pos = do
  Just grid <- res @Grid
  for (getTile pos grid) $ \tile ->
    spawn (Wall, Rel OnTile tile)

spawnWalls :: System ()
spawnWalls = do
  for_ [0 .. gridW - 1] $ \i -> spawnWall (0, i)
  for_ [0 .. gridW - 1] $ \i -> spawnWall (gridH - 1, i)
  for_ [1 .. gridH - 2] $ \i -> spawnWall (i, 0)
  for_ [1 .. gridH - 2] $ \i -> spawnWall (i, gridW - 1)

hasWall :: Entity -> System Bool
hasWall = tileHas @Wall

tileHas :: forall c. (Component c) => Entity -> System Bool
tileHas tile = not . null <$> query [q|Entity / With (c, OnTile -> tile)|]

movePlayerBy :: (Int, Int) -> System ()
movePlayerBy dir = do
  [q|OnTile -> (Pos), Res Grid / With Player|]
    & qmapMaybe (\(pos, Res grid) -> moveBy dir pos.comp grid)
    & qfilterM (const $ (not <$>) . hasWall)
    & qinsert (Rel OnTile)
    & query_

movePlayer :: System ()
movePlayer = do
  c <- Stdin.readLast
  for_ c $ \case
    'w' -> movePlayerBy (-1, 0)
    's' -> movePlayerBy (1, 0)
    'a' -> movePlayerBy (0, -1)
    'd' -> movePlayerBy (0, 1)
    _ -> pure ()

showTile :: Entity -> System Char
showTile tile = do
  player <- tileHas @Player tile
  enemy <- tileHas @Enemy tile
  wall <- tileHas @Wall tile

  pure $
    if
      | player -> '@'
      | wall -> '#'
      | enemy -> '!'
      | otherwise -> '.'

showGrid :: System String
showGrid = do
  Just (Grid tiles) <- res @Grid
  lines <- for tiles $ traverse showTile
  pure $ unlines lines

printGrid :: System ()
printGrid = printClear =<< showGrid

data Rand = Rand (IOGenM StdGen) deriving (Component)

newGen :: System Rand
newGen = Rand <$> (newIOGenM =<< initStdGen)

randomPos :: System (Int, Int)
randomPos = do
  Just (Rand gen) <- res @Rand
  i <- applyIOGen (uniformR (0, gridH - 1)) gen
  j <- applyIOGen (uniformR (0, gridW - 1)) gen
  return (i, j)

randomTile :: System Entity
randomTile = do
  Just grid <- res @Grid
  randomPos <- randomPos

  pure . unwrap $ getTile randomPos grid

data Enemy = Enemy

instance Component Enemy where
  required = require @Cooldown

data Cooldown = Cooldown {timer :: Timer} deriving (Component)

instance Default Cooldown where
  def = Cooldown $ Timer.new 1 Timer.Repeat

spawnEnemy :: System Entity
spawnEnemy = do
  tile <- randomTile
  spawn (Enemy, Rel OnTile tile)

spawnEnemies :: System ()
spawnEnemies = for_ [0 .. 4] $ const spawnEnemy

moveEnemies :: System ()
moveEnemies = do
  Just (From _ playerPos) <- single [q|OnTile -> (Pos) / With Player|]

  [q|OnTile -> (Pos), Res Grid / With Enemy|]
    & qfilterCooldown
    & qdecideEnemyTile playerPos
    & qinsert (Rel OnTile)
    & query_

qdecideEnemyTile :: Pos -> Query System (From Pos, Res Grid) -> Query System Entity
qdecideEnemyTile playerPos x =
  x
    & qtraverse (\_ (pos, grid) -> (,pos,grid) <$> decideEnemyDir pos.comp playerPos)
    & qmapMaybe (\(diff, pos, Res grid) -> moveBy diff pos.comp grid)

qfilterCooldown :: Query System a -> Query System a
qfilterCooldown x = do
  x
    & qextend (,) [q|Cooldown|]
    & qfilterM
      ( \entity (_, Cooldown timer) -> do
          delta <- Time.delta
          let (timer', justFinished) = Timer.tick delta timer
          insert (Cooldown timer') entity
          pure justFinished
      )
    & qmap fst

decideEnemyDir :: Pos -> Pos -> System (Int, Int)
decideEnemyDir (Pos (ex, ey)) (Pos (px, py)) = do
  pure $
    if
      | ex > px -> (-1, 0)
      | ey > py -> (0, -1)
      | ex < px -> (1, 0)
      | ey < py -> (0, 1)
      | otherwise -> (0, 0)

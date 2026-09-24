{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}

{- HLINT ignore "Use newtype instead of data" -}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.List ((!?))
import Data.Text qualified as T
import Data.Traversable
import GHC.Generics (Generic)
import Mischief.ECS.Interval qualified as Interval
import Mischief.ECS.Prelude
import Mischief.ECS.Stdin qualified as Stdin
import Mischief.ECS.Stdout
import Mischief.ECS.Time qualified as Time
import Mischief.ECS.Timer qualified as Timer
import System.Exit (exitSuccess)
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

    systems spawnGrid
      & schedule @Startup

    systems spawnWalls
      & after spawnGrid
      & schedule @Startup

    systems printGrid
      & schedule @Update

    interval <- Interval.start 2000000 spawnCoin

    insertRes =<< newGen

  deps = [dep @PlayerPlugin, dep @EnemyPlugin, dep @TimePlugin]

data PlayerPlugin

instance Plugin PlayerPlugin where
  init = do
    systems spawnPlayer
      & after spawnGrid
      & schedule @Startup

    systems movePlayer
      & schedule @Update

    systems collectCoins
      & after movePlayer
      & schedule @Update

    void $ spawn (Observer onDamage)

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

data Player = Player

instance Component Player where
  required = require @(Health, Coins)

data OnTile = OnTile

instance Component OnTile where
  type IsExclusiveRel OnTile = True

spawnPlayer :: System ()
spawnPlayer = do
  Just grid <- res @Grid
  let Just tile = getTile (5, 5) grid
  void $ spawn (Player, Rel OnTile tile)

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

tileHas :: forall c. (Component c) => Entity -> System Bool
tileHas tile = not . null <$> query [q|Entity / With (c, OnTile -> tile)|]

hasWall :: Entity -> System Bool
hasWall = tileHas @Wall

showTile :: Entity -> System Char
showTile tile = do
  player <- tileHas @Player tile
  enemy <- tileHas @Enemy tile
  wall <- tileHas @Wall tile
  coin <- tileHas @Coin tile

  pure $
    if
      | player -> '@'
      | wall -> '#'
      | enemy -> '!'
      | coin -> '$'
      | otherwise -> '.'

showGrid :: System String
showGrid = do
  Just (Grid tiles) <- res @Grid
  lines <- for tiles $ traverse showTile
  pure $ unlines lines

printGrid :: System ()
printGrid = do
  grid <- showGrid
  health <- showHealth
  coins <- showCoins
  printClear $ health ++ "\n" ++ grid ++ "\n" ++ coins ++ "\n"

movePlayerBy :: (Int, Int) -> System ()
movePlayerBy dir = do
  Just grid <- res @Grid
  [q|OnTile -> (Pos) / With Player|]
    & qmapMaybe (\pos -> moveBy dir pos.comp grid)
    & qfilterM (const tileIsFree)
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

spawnEnemy :: System Entity
spawnEnemy = do
  tile <- randomTile
  spawn (Enemy, Rel OnTile tile)

spawnEnemies :: System ()
spawnEnemies = for_ [0 .. 4] $ const spawnEnemy

data EnemyPlugin

instance Plugin EnemyPlugin where
  init = do
    systems spawnEnemies
      & after spawnGrid
      & schedule @Startup

    systems moveEnemies
      & schedule @Update

    systems tryDamage
      & after moveEnemies
      & after movePlayer
      & schedule @Update

decideEnemyDir :: Pos -> Pos -> System (Int, Int)
decideEnemyDir (Pos (ex, ey)) (Pos (px, py)) = do
  left <- tileAtPosIsFree (ex - 1, ey)
  up <- tileAtPosIsFree (ex, ey - 1)
  right <- tileAtPosIsFree (ex + 1, ey)
  down <- tileAtPosIsFree (ex, ey + 1)

  pure $
    if
      | ex > px && left -> (-1, 0)
      | ey > py && up -> (0, -1)
      | ex < px && right -> (1, 0)
      | ey < py && down -> (0, 1)
      | otherwise -> (0, 0)

moveEnemies :: System ()
moveEnemies = do
  Just (From _ playerPos) <- single [q|OnTile -> (Pos) / With Player|]
  Just grid <- res @Grid

  [q|OnTile -> (Pos) / With Enemy|]
    & qfilterCooldown
    & qdecideEnemyTile grid playerPos
    & qinsert (Rel OnTile)
    & query_

qdecideEnemyTile :: Grid -> Pos -> Query System (From Pos) -> Query System Entity
qdecideEnemyTile grid playerPos x =
  x
    & qtraverse (\_ pos -> (,pos) <$> decideEnemyDir pos.comp playerPos)
    & qmapMaybe (\(diff, pos) -> moveBy diff pos.comp grid)

data Cooldown = Cooldown {timer :: Timer} deriving (Component)

instance Default Cooldown where
  def = Cooldown $ Timer.new 1 Timer.Repeat

qfilterCooldown :: Query System a -> Query System a
qfilterCooldown x = do
  x
    & qextend [qd|Cooldown|] (,)
    & qfilterM
      ( \entity (_, Cooldown timer) -> do
          delta <- Time.delta
          let (timer', justFinished) = Timer.tick delta timer
          insert (Cooldown timer') entity
          pure justFinished
      )
    & qmap fst

tileIsFree :: Entity -> System Bool
tileIsFree tile = do
  wall <- tileHas @Wall tile
  enemy <- tileHas @Enemy tile
  player <- tileHas @Player tile
  pure $ not (wall || enemy || player)

tileAtPosIsFree :: (Int, Int) -> System Bool
tileAtPosIsFree pos = do
  Just grid <- res @Grid
  maybe (pure False) tileIsFree (getTile pos grid)

data Health = Health {hp :: Int} deriving (Component)

instance Default Health where
  def = Health 100

showHealth :: System String
showHealth = do
  Just health <- single [q|Health / With Player|]
  pure $ "Health: " ++ show health.hp

data Damage = Damage {amount :: Int} deriving (Event)

onDamage :: Damage -> System ()
onDamage dmg = do
  [q|Health / With Player, Without Invincible|]
    & qmodify (\(Health x) -> (Health $ max (x - dmg.amount) 0, Invincible))
    & qtap
      ( \e (Health hp, _) -> do
          delay 1000000 $ remove (C @Invincible) e
          when (hp == 0) $ liftIO exitSuccess
      )
    & query_

tryDamage :: System ()
tryDamage = do
  adjacentEnemies <-
    [q|OnTile -> (Pos) / With Player|]
      & qjoin (\pos -> [q|OnTile -> (Pos) / With Enemy|] & qfilter (\a -> isAdjacent a.comp pos.comp)) (,)
      & query

  unless (null adjacentEnemies) $ do
    trigger $ Damage 5

isAdjacent :: Pos -> Pos -> Bool
isAdjacent (Pos (x1, y1)) (Pos (x2, y2)) =
  let dx = abs (x1 - x2)
      dy = abs (y1 - y2)
   in (dx == 1 && dy == 0) || (dx == 0 && dy == 1)

data Invincible = Invincible deriving (Component)

data Coin = Coin Int deriving (Component)

spawnCoin :: System ()
spawnCoin = do
  tile <- randomTile
  free <- tileIsFree tile
  if free
    then
      void $ spawn (Coin 5, Rel OnTile tile)
    else
      spawnCoin

data Coins = Coins Int deriving (Component, Generic, Default)

showCoins :: System String
showCoins = do
  Just (Coins c) <- single $ [q|Coins|]
  pure $ "Coins: " ++ show c

collectCoins :: System ()
collectCoins = do
  players <- query [q|Entity, OnTile -> (Entity), Coins / With Player|]
  for_ players $ \(player, From _ tile, Coins coins) -> do
    collected <-
      [q|Coin / With OnTile -> tile|]
        & qtap (\e _ -> despawn e)
        & query

    insert (Coins $ foldr (\(Coin x) -> (+ x)) coins collected) player

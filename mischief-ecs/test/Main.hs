{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{- HLINT ignore "Use newtype instead of data" -}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{- HLINT ignore "Use zipWith" -}

module Main where

import Control.Monad (forever, replicateM, unless, void, when)
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.List ((!?))
import Data.Traversable
import GHC.IO.Handle
import Mischief.ECS
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Stdin qualified as Stdin
import Mischief.ECS.Stdout
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.Timer (Timer)
import Mischief.ECS.Timer qualified as Timer
import System.IO
import System.Random
import System.Random.Stateful
import Prelude hiding (Left, Right)

data Likes = Likes Int deriving (Show)

instance Component Likes where
  hooks = Hooks.relCleanupRemove

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    -- Stdin.init
    -- Systems.add Startup (spawnGrid, spawnWalls)
    -- Systems.add Update printGrid

    -- insertRes =<< newGen
    f

-- plugins _ = plug (PlayerPlugin, EnemyPlugin, TimePlugin)

data PlayerPlugin = PlayerPlugin deriving (Eq)

instance Plugin PlayerPlugin where
  init _ = do
    Systems.add Startup $ spawnPlayer `after` spawnGrid
    Systems.add Update movePlayer

data EnemyPlugin = EnemyPlugin deriving (Eq)

instance Plugin EnemyPlugin where
  init _ = do
    Systems.add Startup spawnEnemies
    Systems.add Update moveEnemies

data Tile = Tile deriving (Component)

newtype Pos = Pos {pos :: (Int, Int)} deriving (Component, Show)

data Grid = Grid [[Entity]] deriving (Component)

getTile :: (Int, Int) -> System (Maybe Entity)
getTile (x, y) = do
  grid <- res @Grid
  pure $ do
    Grid tiles <- grid
    line <- tiles !? x
    line !? y

gridH :: Int
gridH = 10

gridW :: Int
gridW = 20

spawnGrid :: System ()
spawnGrid = do
  tiles <- for [0 .. gridH - 1] $ \i -> for [0 .. gridW - 1] $ \j ->
    spawn (Tile, Pos (i, j))

  insertRes $ Grid tiles

moveBy :: (Int, Int) -> Entity -> System (Maybe Entity)
moveBy (x, y) entity = do
  Just (Pos (x', y')) <- [g|*Pos|] entity
  getTile (x' + x, y' + y)

data Player = Player deriving (Component)

data OnTile = OnTile

instance Component OnTile where
  type RelExclusivity OnTile = Exclusive

spawnPlayer :: System ()
spawnPlayer = do
  Just tile <- getTile (5, 5)
  void $ spawn (Player, Rel OnTile tile)

data Wall = Wall deriving (Component)

spawnWall :: (Int, Int) -> System Entity
spawnWall pos = do
  Just tile <- getTile pos
  spawn (Wall, Rel OnTile tile)

spawnWalls :: System ()
spawnWalls = do
  for_ [0 .. gridW - 1] $ \i -> spawnWall (0, i)
  for_ [0 .. gridW - 1] $ \i -> spawnWall (gridH - 1, i)
  for_ [1 .. gridH - 2] $ \i -> spawnWall (i, 0)
  for_ [1 .. gridH - 2] $ \i -> spawnWall (i, gridW - 1)

showTile :: Entity -> System Char
showTile tile = do
  entities <- query' (Has @Player, Has @Wall, Has @Enemy) (With (R @OnTile tile))
  pure $ case entities of
    ((_, _, True) : _) -> '!'
    ((True, _, _) : _) -> '@'
    ((_, True, _) : _) -> '#'
    _ -> '.'

showGrid :: System String
showGrid = do
  Just (Grid tiles) <- res @Grid
  lines <- for tiles $ traverse showTile
  return $ unlines lines

printGrid :: System ()
printGrid = printClear =<< showGrid

movePlayer :: System ()
movePlayer = do
  c <- Stdin.readLast
  for_ c $ \case
    'w' -> movePlayerBy (-1, 0)
    's' -> movePlayerBy (1, 0)
    'a' -> movePlayerBy (0, -1)
    'd' -> movePlayerBy (0, 1)
    _ -> pure ()

movePlayerBy :: (Int, Int) -> System ()
movePlayerBy dir = do
  Just player <- single' E (With (C @Player))

  Just rel <- get (R @OnTile Any) player
  let tile = rel.target

  newTile <- moveBy dir tile

  for_ newTile $ \t ->
    hasWall t >>= flip unless (insert (Rel OnTile t) player)

hasWall :: Entity -> System Bool
hasWall tile = not . null <$> [q|Entity / With (Wall, OnTile -> tile)|]

data Enemy = Enemy

instance Component Enemy where
  required = require @Cooldown

data Cooldown = Cooldown {timer :: Timer} deriving (Component)

instance Default Cooldown where
  def = Cooldown $ Timer.new 0.5 Timer.Repeat

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
randomTile = unwrap <$> (getTile =<< randomPos)

spawnEnemy :: System Entity
spawnEnemy = do
  tile <- randomTile
  spawn (Enemy, Rel OnTile tile)

spawnEnemies :: System ()
spawnEnemies = for_ [0 .. 4] $ const spawnEnemy

moveEnemies :: System ()
moveEnemies = do
  Just tile <- single' (R @OnTile Any) (With (C @Player))
  Just (Pos (px, py)) <- get (Val (C @Pos)) tile.target

  delta <- deltaTime

  enemies <- query' (E, R @OnTile Any, C @Cooldown) (With (C @Enemy))
  for_ enemies $ \(enemy, tile, cooldown) -> do
    let (timer, finished) = Timer.tick delta cooldown.timer
    set cooldown $ Cooldown timer

    when finished $ do
      Just (Pos (x, y)) <- get (Val (C @Pos)) tile.target

      let diff = case (x > px, y > py, x < px, y < py) of
            (True, _, _, _) -> (-1, 0)
            (_, True, _, _) -> (0, -1)
            (_, _, True, _) -> (1, 0)
            (_, _, _, True) -> (0, 1)
            _ -> (0, 0)

      moveBy diff tile.target
        >>= traverse_
          ( \t ->
              insert (Rel OnTile t) enemy
          )

data R1 = R1 deriving (Component)

data R2 = R2

instance Component R2 where
  type RelExclusivity R2 = Exclusive

data C1 = C1 deriving (Component, Show)

f :: System ()
f = do
  a <- spawn (Name "A", C1)
  b <- spawn (Name "B", C1)
  c <- spawn (Name "C")

  d <- spawn (Rel R1 a, Rel R1 b, Rel R1 c)
  e <- spawn (Rel R1 c)
  f <- spawn (Rel R1 a, Rel R1 c)

  x <- query (R @R1 (Q (C @C1, C @Name)))
  info $ text x
{- HLINT ignore "Use newtype instead of data" -}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{- HLINT ignore "Use zipWith" -}

module Main where

import Control.Monad (forever, replicateM, unless, void, when)
import Control.Monad.IO.Class
import Data.Foldable
import Data.List ((!?))
import Data.Traversable
import GHC.IO.Handle
import Mischief.ECS
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Stdin qualified as Stdin
import Mischief.ECS.Stdout
import Mischief.ECS.Systems qualified as Systems
import System.IO
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
    Stdin.init
    Systems.add Startup (spawnGrid, spawnWalls)
    Systems.add Update printGrid

  plugins _ = plug PlayerPlugin

data PlayerPlugin = PlayerPlugin deriving (Eq)

instance Plugin PlayerPlugin where
  init _ = do
    Systems.add Startup $ spawnPlayer `after` spawnGrid
    Systems.add Update movePlayer

data Tile = Tile deriving (Component)

data Pos = Pos {pos :: (Int, Int)} deriving (Component)

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
  isExclusiveRel = True

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
  entities <- query' (Has @Player, Has @Wall) (With (R @OnTile tile))
  pure $ case entities of
    ((True, _) : _) -> '@'
    ((_, True) : _) -> '#'
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

  Just [rel] <- get (R @OnTile Any) player
  let tile = rel.target

  newTile <- moveBy dir tile

  for_ newTile $ \t ->
    hasWall t >>= flip unless (insert (Rel OnTile t) player)

hasWall :: Entity -> System Bool
hasWall tile = not . null <$> [q|Entity / With (Wall, OnTile -> tile)|]

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
import Mischief.ECS.Timer (Timer)
import Mischief.ECS.Timer qualified as Timer
import Mischief.ECS.World.Query
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
    systems spawnGrid
      & schedule Startup

    systems spawnWalls
      & after spawnGrid
      & schedule Startup

  deps = [dep @PlayerPlugin]

data PlayerPlugin

instance Plugin PlayerPlugin where
  init = do
    systems spawnPlayer
      & after spawnGrid
      & schedule Startup

    systems movePlayer
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
  wall <- tileHas @Wall tile

  pure $
    if
      | player -> '@'
      | wall -> '#'
      | otherwise -> '.'

showGrid :: System String
showGrid = do
  Just (Grid tiles) <- res @Grid
  lines <- for tiles $ traverse showTile
  pure $ unlines lines

printGrid :: System ()
printGrid = printClear =<< showGrid

movePlayerBy :: (Int, Int) -> System ()
movePlayerBy dir = do
  Just grid <- res @Grid
  [q|OnTile -> (Pos) / With Player|]
    & qmapMaybe (\pos -> moveBy dir pos.comp grid)
    & qfilterM (\_ newTile -> not <$> hasWall newTile)
    & qinsert (\newTile -> Rel OnTile newTile)
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
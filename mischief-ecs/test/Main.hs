{- HLINT ignore "Use newtype instead of data" -}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{- HLINT ignore "Use zipWith" -}

module Main where

import Control.Monad (replicateM, void)
import Data.Foldable
import Data.List ((!?))
import Data.Traversable
import Mischief.ECS
import Mischief.ECS.Hooks qualified as Hooks
import Mischief.ECS.Systems qualified as Systems
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
    Systems.add Startup $ spawnPlayer `after` spawnGrid
    Systems.add Startup spawnGrid

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

spawnGrid :: System ()
spawnGrid = do
  tiles <- for [0 .. 10] $ \i -> for [0 .. 10] $ \j ->
    spawn (Tile, Pos (i, j))

  insertRes $ Grid tiles

up :: Entity -> System (Maybe Entity)
up entity = do
  Just (Pos (x, y)) <- get (Val (C @Pos)) entity
  getTile (x - 1, y)

down :: Entity -> System (Maybe Entity)
down entity = do
  Just (Pos (x, y)) <- get (Val (C @Pos)) entity
  getTile (x + 1, y)

left :: Entity -> System (Maybe Entity)
left entity = do
  Just (Pos (x, y)) <- get (Val (C @Pos)) entity
  getTile (x, y - 1)

right :: Entity -> System (Maybe Entity)
right entity = do
  Just (Pos (x, y)) <- get (Val (C @Pos)) entity
  getTile (x, y + 1)

data Player = Player deriving (Component)

data OnTile = OnTile

instance Component OnTile where
  isExclusiveRel = True

spawnPlayer :: System ()
spawnPlayer = do
  Just tile <- getTile (5, 5)
  void $ spawn (Player, Rel OnTile tile)

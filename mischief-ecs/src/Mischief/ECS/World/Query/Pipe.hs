{-# OPTIONS_GHC -Wno-partial-fields #-}

module Mischief.ECS.World.Query.Pipe where

import Control.Monad (filterM, void)
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable
import Data.Maybe
import Data.Traversable
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Utils

qmapM :: (Bundle b) => (Entity -> out -> System b) -> Query out -> Query b
qmapM f x =
  ChangeQuery
    x
    ( \a -> do
        for a $ \(e, a) -> do
          b <- f e a
          insert b e
          pure (e, b)
    )

qmap :: (Bundle b) => (out -> b) -> Query out -> Query b
qmap f x =
  ChangeQuery
    x
    ( \a -> do
        for a $ \(e, a) -> do
          let b = f a
          insert b e
          pure (e, b)
    )

qfilter :: (out -> Bool) -> Query out -> Query out
qfilter f x = ChangeQuery x (pure . filter (\(_, x) -> f x))

data Position = Position Int deriving (Component)

data Velocity = Velocity Int deriving (Component)

data TC = TC Int deriving (Component)

test :: System ()
test = do
  let x = mkQuery' (C @TC, C @TC, R @TC Any) NoFilter
  let f = qfilter (const True) . qmap (\(_, _, a) -> (Rel (TC 5) (undefined :: Entity), TC 10)) $ x

  let e = undefined :: Entity

  x <- query . qfilter (\(Position x, _, _) -> x > 5) $ mkQuery (C @Position, C @Velocity, R @Velocity e)

  qrun
    . qmap (\(Position x, Velocity y) -> Velocity (x + y))
    $ mkQuery (C @Position, C @Velocity)

  y <- get e $ mkQuery (C @Position)

  qrun . qmapM (\e _ -> remove (C @Position) e) $ mkQuery (C @Position)

  undefined

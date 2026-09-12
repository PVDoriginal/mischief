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
import Mischief.ECS.World.Query.TH
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Utils

qmapM :: (Bundle b) => (Entity -> out -> System b) -> Query out System -> Query b System
qmapM f x =
  ChangeQuery
    x
    ( \a -> do
        for a $ \(e, a) -> do
          b <- f e a
          insert b e
          pure (e, b)
    )

qmap :: (Bundle b) => (out -> b) -> Query out System -> Query b System
qmap f x =
  ChangeQuery
    x
    ( \a -> do
        for a $ \(e, a) -> do
          let b = f a
          insert b e
          pure (e, b)
    )

qfilterM :: (MonadSystem w s) => (Entity -> out -> s Bool) -> Query out s -> Query out s
qfilterM f x = ChangeQuery x (filterM (uncurry f))

qfilter :: (MonadSystem w s) => (out -> Bool) -> Query out s -> Query out s
qfilter f x = ChangeQuery x (pure . filter (\(_, x) -> f x))

check :: (MonadSystem w m) => QueryFilter EntityFilter -> Entity -> m Bool
check NoFilter _ = pure True
check (With x) e = do
  world <- unsafeGetWorld
  liftIO $ filterEntity (With x) world e
check (Without x) e = do
  world <- unsafeGetWorld
  liftIO $ filterEntity (Without x) world e
check (Added x) e = added x e
check (Changed x) e = changed x e
check (And a b) e = (&&) <$> check a e <*> check b e
check (Or a b) e = (||) <$> check a e <*> check b e
check (Not a) e = not <$> check a e

qcheck :: (MonadSystem w s) => QueryFilter EntityFilter -> Query out s -> Query out s
qcheck f x = ChangeQuery x $ filterM (\(e, _) -> check f e)

data Position = Position Int deriving (Component)

data Velocity = Velocity Int deriving (Component)

data TC = TC Int deriving (Component)

data RenderDevice = RenderDevice deriving (Component)

data Likes = Likes deriving (Component)

test :: System ()
test = do
  let x = mkQuery' (C @TC, C @TC, R @TC Any) NoFilter
  let f = qfilter (const True) . qmap (\(_, _, a) -> (Rel (TC 5) (undefined :: Entity), TC 10)) $ x

  let e = undefined :: Entity

  x <-
    query
      . qcheck [qf|Changed Velocity, With Velocity|]
      . qfilter (\(Position x, _, _) -> x > 5)
      $ [q|Position, Velocity, Position -> e|]

  qrun
    . qmap (\(Velocity v, positions) -> map (\(From x (Position p, a)) -> From x (Position $ p + v, a)) positions)
    -- \$ mkQuery (C @Velocity, R @Likes (Q (C @Position, R @Likes (Q (C @Position)))))
    $ [q|Velocity, Likes -> (Position, Likes -> (Position)) / With Position|]

  y <- get e $ mkQuery (C @Position)

  qrun . qmapM (\e _ -> remove (C @Position) e) $ mkQuery (C @Position)

  undefined

-- test' :: ParSystem ()
-- test' = do
--   let e = undefined :: Entity
--   x <- query . qfilter (\(Position x, _, _) -> x > 5) $ mkQuery (C @Position, C @Velocity, R @Velocity e)

--   qrun
--     . qmap (\(Position x, Velocity y) -> Velocity (x + y))
--     $ mkQuery (C @Position, C @Velocity)

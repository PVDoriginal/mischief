{-# OPTIONS_GHC -Wno-partial-fields #-}

module Mischief.ECS.World.Query.Pipe where

import Control.Monad (filterM, void, (<=<))
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import GHC.Stack
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.Log
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
  MapQuery
    x
    ( \e a -> do
        y <- f e a
        insert y e
        pure y
    )

qmap :: (Bundle b) => (out -> b) -> Query out System -> Query b System
qmap f x =
  MapQuery
    x
    ( \e a -> do
        let y = f a
        insert y e
        pure y
    )

qtraverse :: (MonadSystem w s) => (Entity -> a -> s b) -> Query a s -> Query b s
qtraverse f x = MapQuery x f

qtraverse_ :: (MonadSystem w s) => (Entity -> a -> s b) -> Query a s -> Query a s
qtraverse_ f x = DoQuery x (\e x -> void (f e x))

qinsert :: (Bundle b) => (a -> b) -> Query a System -> Query a System
qinsert f x = DoQuery x (\e a -> insert (f a) e)

qinsertNew :: (Bundle b) => (a -> b) -> Query a System -> Query a System
qinsertNew f x = DoQuery x (\e a -> insertNew (f a) e)

qinsertIfNeq :: (BundleEq b) => (a -> b) -> Query a System -> Query a System
qinsertIfNeq f x = DoQuery x (\e a -> insertIfNeq (f a) e)

qfilterM :: (Entity -> out -> s Bool) -> Query out s -> Query out s
qfilterM f x = FilterQuery x f

qfilter :: (MonadSystem w s) => (out -> Bool) -> Query out s -> Query out s
qfilter f x = FilterQuery x (\_ x -> pure (f x))

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

qcheck :: (MonadSystem w s) => QueryFilter EntityFilter -> Query a s -> Query a s
qcheck f x =
  FilterQuery
    x
    (\e _ -> check f e)

qappend1 :: (MonadSystem w s) => (a -> Maybe Entity) -> (a -> From b -> c) -> Query b s -> Query a s -> Query c s
qappend1 f f' y x =
  mapFilterQuery
    x
    ( \_ x -> do
        let entity = f x
        case entity of
          Nothing -> pure Nothing
          Just entity -> do
            y <- get entity y
            case y of
              Nothing -> pure Nothing
              Just y -> pure $ Just (f' x (From entity y))
    )

qappend1M :: (MonadSystem w s) => (Entity -> a -> s (Maybe Entity)) -> (a -> From b -> c) -> Query b s -> Query a s -> Query c s
qappend1M f f' y x =
  mapFilterQuery
    x
    ( \e x -> do
        entity <- f e x
        case entity of
          Nothing -> pure Nothing
          Just entity -> do
            y <- get entity y
            case y of
              Nothing -> pure Nothing
              Just y -> do
                pure . Just $ f' x (From entity y)
    )

qappend :: (MonadSystem w s) => (a -> [Entity]) -> (a -> [From b] -> c) -> Query b s -> Query a s -> Query c s
qappend f f' y x =
  mapFilterQuery
    x
    ( \_ x -> do
        let entities = f x
        y <- catMaybes <$> mapM (\e -> fmap (e,) <$> get e y) entities
        case y of
          [] -> pure Nothing
          y -> pure $ Just $ f' x (map (uncurry From) y)
    )

qappendM :: (MonadSystem w s) => (Entity -> a -> s [Entity]) -> (a -> [From b] -> c) -> Query b s -> Query a s -> Query c s
qappendM f f' y x =
  mapFilterQuery
    x
    ( \e x -> do
        entities <- f e x
        y <- catMaybes <$> mapM (\e -> fmap (e,) <$> get e y) entities
        case y of
          [] -> pure Nothing
          y -> pure . Just $ f' x (map (uncurry From) y)
    )

qrelateOne :: (MonadSystem w s) => (Entity -> s (Maybe Entity)) -> (a -> From b -> c) -> Query b s -> Query a s -> Query c s
qrelateOne f f' y x =
  mapFilterQuery
    x
    ( \e x -> do
        entity <- f e
        case entity of
          Nothing -> pure Nothing
          Just entity -> do
            y <- get entity y
            case y of
              Nothing -> pure Nothing
              Just y -> pure $ Just (f' x (From entity y))
    )

qrelateMany :: (MonadSystem w s, Foldable t) => (Entity -> s (t Entity)) -> (a -> [From b] -> c) -> Query b s -> Query a s -> Query c s
qrelateMany f f' y x =
  mapFilterQuery
    x
    ( \e x -> do
        entities <- f e
        y <- catMaybes <$> mapM (\e -> fmap (e,) <$> get e y) (toList entities)
        case y of
          [] -> pure Nothing
          y -> pure . Just $ f' x (map (uncurry From) y)
    )

qjoin :: (MonadSystem w s) => (a -> b -> Bool) -> (a -> [From b] -> c) -> Query b s -> Query a s -> Query c s
qjoin f f' a b =
  mapFilterQuery
    b
    ( \_ x -> do
        y <- query (qentity a)
        let b = filter (f x . snd) y
        let c = f' x (map (uncurry From) b)
        pure $ Just c
    )

qjoinM :: (MonadSystem w s) => (a -> b -> Bool) -> (Entity -> a -> [From b] -> s c) -> Query b s -> Query a s -> Query c s
qjoinM f f' a b =
  mapFilterQuery
    b
    ( \e x -> do
        y <- query (qentity a)
        let b = filter (f x . snd) y
        c <- f' e x (map (uncurry From) b)
        pure $ Just c
    )

qextend :: (MonadSystem w s) => (a -> b -> c) -> Query b s -> Query a s -> Query c s
qextend f y x =
  mapFilterQuery
    x
    ( \e x -> do
        y <- get e y
        case y of
          Nothing -> pure Nothing
          Just y -> pure $ Just $ f x y
    )

qextendM :: (MonadSystem w s) => (Entity -> a -> b -> s c) -> Query b s -> Query a s -> Query c s
qextendM f y x =
  mapFilterQuery
    x
    ( \e x -> do
        y <- get e y
        case y of
          Nothing -> pure Nothing
          Just y -> Just <$> f e x y
    )

qinfo :: (HasCallStack, MonadSystem w s) => (a -> Text) -> Query a s -> Query a s
qinfo f a = withFrozenCallStack $ DoQuery a (\_ x -> info (f x))

qwarn :: (HasCallStack, MonadSystem w s) => (a -> Text) -> Query a s -> Query a s
qwarn f a = withFrozenCallStack $ DoQuery a (\_ x -> warn (f x))

qerr :: (HasCallStack, MonadSystem w s) => (a -> Text) -> Query a s -> Query a s
qerr f a = withFrozenCallStack $ DoQuery a (\_ x -> err (f x))

qentity :: (MonadSystem w s) => Query a s -> Query (Entity, a) s
qentity = qextend (\a b -> (b, a)) (mkQuery E)

-- data Position = Position Int deriving (Component, Num)

-- data Velocity = Velocity Int deriving (Component, Show)

-- data TC = TC Int deriving (Component)

-- data RenderDevice = RenderDevice deriving (Component)

-- data Likes = Likes deriving (Component)

-- data Player = Player deriving (Component)

-- data Name = Name String deriving (Component, Show)

-- data Child = Child deriving (Component, Show)

-- test :: System ()
-- test = do
--   query_
--     . qmap (\(Position x, Velocity y) -> Position (x + y))
--     . qfilter (\(_, Velocity y) -> y > 5)
--     $ [q|Position, Velocity / With Player|]

--   query_
--     . qmapM
--       ( \_ (name, vel) -> do
--           info $ "My name is " <> text name
--           info $ "My velocity is " <> text vel
--       )
--     . qextend (,) [q|Velocity|]
--     $ [q|Name|]

--   query_
--     . qmap (\(parentPos, children) -> map (\(From child pos) -> From child (pos + parentPos)) children)
--     . qcheck [f|Changed Position|]
--     $ [q|Position, Child -> (Position)|]

--   [q|Velocity|]
--     & qjoin (\(Velocity v) (Position p) -> v == p) (,) [q|Position|]
--     & qmap (\(Velocity v, positions) -> map (\(From e (Position p)) -> From e (Position (p + v))) positions)
--     & query_

--   x <- query $ qcheck [f|Changed Position|] [q|Position, Child -> (Position)|]

--   let player = undefined :: Entity
--   y <- get player [q|Name|]

--   undefined

-- -- test' :: ParSystem ()
-- -- test' = do
-- --   let e = undefined :: Entity
-- --   x <- query . qfilter (\(Position x, _, _) -> x > 5) $ mkQuery (C @Position, C @Velocity, R @Velocity e)

-- --   qrun
-- --     . qmap (\(Position x, Velocity y) -> Velocity (x + y))
-- --     $ mkQuery (C @Position, C @Velocity)

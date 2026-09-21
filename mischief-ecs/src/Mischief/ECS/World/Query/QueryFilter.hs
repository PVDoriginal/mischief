{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Query.QueryFilter where

import Data.Data
import Data.Maybe
import GHC.Base (eqWord#, isTrue#)
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.World
import Mischief.ECS.World.Query.Markers

-- import Mischief.ECS.World.Query.Queryable

-- newtype QueryFilters = QueryFilters [QueryFilter] deriving newtype (Semigroup)

qfChangedF :: ComponentTicks -> Tick -> Tick -> Bool
qfChangedF ticks lastSystemTick currentSystemTick = ticks.changed >= lastSystemTick && ticks.changed < currentSystemTick

qfAddedF :: ComponentTicks -> Tick -> Tick -> Bool
qfAddedF ticks lastSystemTick currentSystemTick = ticks.added >= lastSystemTick && ticks.added < currentSystemTick

data FilterType = ArchetypeFilter | EntityFilter

data QueryFilter (f :: FilterType) where
  NoFilter :: QueryFilter f
  With :: (ToFilterComponent a) => a -> QueryFilter f
  Without :: (ToFilterComponent a) => a -> QueryFilter f
  Changed :: (ToFilterComponent a) => a -> QueryFilter EntityFilter
  Added :: (ToFilterComponent a) => a -> QueryFilter EntityFilter
  Not :: QueryFilter f -> QueryFilter f
  And :: QueryFilter f -> QueryFilter f -> QueryFilter f
  Or :: QueryFilter f -> QueryFilter f -> QueryFilter f

newtype FilterComponent = FilterComponent {inner :: (TypeRep, Maybe Entity, Maybe Any)}

class ToFilterComponent a where
  toFilterComponent :: a -> FilterComponent

instance (Component c) => ToFilterComponent (C c) where
  toFilterComponent _ = FilterComponent (typeRep $ Proxy @c, Nothing, Nothing)

instance (Component c) => ToFilterComponent (R c Entity) where
  toFilterComponent (R entity) = FilterComponent (typeRep $ Proxy @c, Just entity, Nothing)

instance (Component c) => ToFilterComponent (R c Any) where
  toFilterComponent _ = FilterComponent (typeRep $ Proxy @c, Nothing, Just Any)

instance Semigroup (QueryFilter f) where
  (<>) :: QueryFilter f -> QueryFilter f -> QueryFilter f
  (<>) = And

filterArchetype' :: FilterComponent -> World -> [ComponentId] -> IO Bool
filterArchetype' (FilterComponent (c, entity, Nothing)) world components = do
  component <- fmap (setCompIdTarget entity) <$> getComponentId c world.components
  return $ case component of
    Nothing -> False
    Just component -> component `elem` components
filterArchetype' (FilterComponent (c, _, Just _)) world components = do
  component <- getComponentId c world.components
  case component of
    Nothing -> pure False
    Just (ComponentId (# id, _ #)) -> do
      return $ any (\(ComponentId (# id', a #)) -> isJust a && isTrue# (eqWord# id' id)) components

filterArchetype :: QueryFilter ArchetypeFilter -> World -> [ComponentId] -> IO Bool
filterArchetype NoFilter _ _ = pure True
filterArchetype (With a) b c = filterArchetype' (toFilterComponent a) b c
filterArchetype (Without a) b c = not <$> filterArchetype' (toFilterComponent a) b c
filterArchetype (Not a) b c = not <$> filterArchetype a b c
filterArchetype (And a0 a1) b c = (&&) <$> filterArchetype a0 b c <*> filterArchetype a1 b c
filterArchetype (Or a0 a1) b c = (||) <$> filterArchetype a0 b c <*> filterArchetype a1 b c

preprocessFilter :: QueryFilter f -> QueryFilter f
preprocessFilter = propagateQFNot

propagateQFNot :: QueryFilter f -> QueryFilter f
propagateQFNot (Not NoFilter) = NoFilter
propagateQFNot (Not (Not a)) = propagateQFNot a
propagateQFNot (Not (a `And` b)) = propagateQFNot (Not a) `Or` propagateQFNot (Not b)
propagateQFNot (Not (a `Or` b)) = propagateQFNot (Not a) `And` propagateQFNot (Not b)
propagateQFNot (a `And` b) = propagateQFNot a `And` propagateQFNot b
propagateQFNot (a `Or` b) = propagateQFNot a `Or` propagateQFNot b
propagateQFNot x = x

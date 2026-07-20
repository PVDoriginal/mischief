{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Query.QueryFilter where

import Data.Data
import Data.Maybe
import Data.Set qualified as Set
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Entities
import Mischief.ECS.World
import Mischief.ECS.World.Query.Queryable

-- newtype QueryFilters = QueryFilters [QueryFilter] deriving newtype (Semigroup)

qfChangedF :: ComponentTicks -> Tick -> Tick -> Bool
qfChangedF ticks lastSystemTick currentSystemTick = ticks.changed >= lastSystemTick && ticks.changed < currentSystemTick

qfAddedF :: ComponentTicks -> Tick -> Tick -> Bool
qfAddedF ticks lastSystemTick currentSystemTick = ticks.added >= lastSystemTick && ticks.added < currentSystemTick

data QueryFilter
  = NoFilter
  | QFWith (TypeRep, Maybe Entity)
  | QFWithRelAny TypeRep
  | QFChanged (TypeRep, Maybe Entity) (ComponentTicks -> Tick -> Tick -> Bool)
  | QFChangedRelAny TypeRep (ComponentTicks -> Tick -> Tick -> Bool)
  | QFCheckRaw (TypeRep, ErasedCheck)
  | QFNot QueryFilter
  | QFAnd QueryFilter QueryFilter
  | QFOr QueryFilter QueryFilter

instance Semigroup QueryFilter where
  (<>) :: QueryFilter -> QueryFilter -> QueryFilter
  (<>) = QFAnd

instance {-# OVERLAPPING #-} EraseIntoStorage QueryFilter QueryFilter where
  erase = id

instance (IntoQueryFilter q) => EraseIntoStorage q QueryFilter where
  erase = intoQueryFilter

data ErasedCheck where
  ErasedCheck :: (Component c) => (c -> Bool) -> ErasedCheck

instance Show ErasedCheck where
  show _ = "erased check"

and' :: [QueryFilter] -> QueryFilter
and' [x] = x
and' x = foldr QFAnd NoFilter x

filterArchetype :: QueryFilter -> [ComponentId] -> World -> IO Bool
filterArchetype NoFilter _ _ = return True
filterArchetype (QFWith (x, entity)) components world = do
  component <- fmap (\x -> x {entity}) <$> getComponentId x world.components
  return $ case component of
    Nothing -> False
    Just component -> component `elem` components
filterArchetype (QFWithRelAny x) components world = do
  component <- getComponentId x world.components
  case component of
    Nothing -> pure False
    Just (ComponentId {id}) -> do
      return $ any (\c -> isJust c.entity && c.id == id) components
filterArchetype (a `QFAnd` b) c w = do
  x <- filterArchetype a c w
  y <- filterArchetype b c w
  return $ x && y
filterArchetype (a `QFOr` b) c w = do
  x <- filterArchetype a c w
  y <- filterArchetype b c w
  return $ x || y
filterArchetype (QFNot a) c w = not <$> filterArchetype a c w
filterArchetype _ _ _ = pure True

-- | Extracts archetype-level filters from the bigger filter where possible, to be applied at the start of querying for better performance.
extractArchetypeFilters :: QueryFilter -> (QueryFilter, QueryFilter)
extractArchetypeFilters NoFilter = (NoFilter, NoFilter)
extractArchetypeFilters (QFWith x) = (NoFilter, QFWith x)
extractArchetypeFilters (QFWithRelAny x) = (NoFilter, QFWithRelAny x)
extractArchetypeFilters (QFChanged x f) = (QFChanged x f, NoFilter)
extractArchetypeFilters (QFChangedRelAny x f) = (QFChangedRelAny x f, NoFilter)
extractArchetypeFilters (QFCheckRaw x) = (QFCheckRaw x, NoFilter)
extractArchetypeFilters (a `QFAnd` b) = (filter1 `QFAnd` filter2, res1 `QFAnd` res2)
  where
    (filter1, res1) = extractArchetypeFilters a
    (filter2, res2) = extractArchetypeFilters b
extractArchetypeFilters (a `QFOr` b) =
  if isArchetypeFilter a && isArchetypeFilter b
    then
      (filter1 `QFOr` filter2, res1 `QFOr` res2)
    else
      (a `QFOr` b, NoFilter)
  where
    (filter1, res1) = extractArchetypeFilters a
    (filter2, res2) = extractArchetypeFilters b
extractArchetypeFilters (QFNot a) = (QFNot x, QFNot y)
  where
    (x, y) = extractArchetypeFilters a

isArchetypeFilter :: QueryFilter -> Bool
isArchetypeFilter NoFilter = True
isArchetypeFilter (QFChanged _ _) = False
isArchetypeFilter (QFChangedRelAny _ _) = False
isArchetypeFilter (QFWith _) = True
isArchetypeFilter (QFWithRelAny _) = True
isArchetypeFilter (a `QFAnd` b) = isArchetypeFilter a || isArchetypeFilter b
isArchetypeFilter (a `QFOr` b) = isArchetypeFilter a && isArchetypeFilter b
isArchetypeFilter (QFCheckRaw _) = False
isArchetypeFilter (QFNot a) = isArchetypeFilter a

preprocessFilter :: QueryFilter -> QueryFilter
preprocessFilter = propagateQFNot

propagateQFNot :: QueryFilter -> QueryFilter
propagateQFNot (QFNot NoFilter) = NoFilter
propagateQFNot (QFNot (QFNot a)) = propagateQFNot a
propagateQFNot (QFNot (a `QFAnd` b)) = propagateQFNot (QFNot a) `QFOr` propagateQFNot (QFNot b)
propagateQFNot (QFNot (a `QFOr` b)) = propagateQFNot (QFNot a) `QFAnd` propagateQFNot (QFNot b)
propagateQFNot (a `QFAnd` b) = propagateQFNot a `QFAnd` propagateQFNot b
propagateQFNot (a `QFOr` b) = propagateQFNot a `QFOr` propagateQFNot b
propagateQFNot x = x

class IntoQueryFilter qf where
  intoQueryFilter :: qf -> QueryFilter

data With c = With

instance (BundleTypes c) => IntoQueryFilter (With c) where
  intoQueryFilter _ = and' $ map (\x -> QFWith (x, Nothing)) (Set.toList $ types (Proxy @c))

data Without c = Without

instance (BundleTypes c) => IntoQueryFilter (Without c) where
  intoQueryFilter _ = and' $ map (\x -> QFNot $ QFWith (x, Nothing)) (Set.toList $ types (Proxy @c))

newtype WithR c e = WithR e

instance (BundleTypes c) => IntoQueryFilter (WithR c Entity) where
  intoQueryFilter (WithR target) = and' $ map (\x -> QFWith (x, Just target)) (Set.toList $ types (Proxy @c))

instance (BundleTypes c) => IntoQueryFilter (WithR c Any) where
  intoQueryFilter _ = and' $ map QFWithRelAny (Set.toList $ types (Proxy @c))

newtype Not c = Not c

instance (IntoQueryFilter q) => IntoQueryFilter (Not q) where
  intoQueryFilter (Not a) = QFNot $ intoQueryFilter a

data Or a b = Or a b

(|.) :: a -> b -> Or a b
(|.) = Or

instance (IntoQueryFilter a, IntoQueryFilter b) => IntoQueryFilter (a `Or` b) where
  intoQueryFilter (Or a b) = QFOr (intoQueryFilter a) (intoQueryFilter b)

data Changed c = Changed

instance (BundleTypes c) => IntoQueryFilter (Changed c) where
  intoQueryFilter _ = and' $ map (\x -> QFWith (x, Nothing) `QFAnd` QFChanged (x, Nothing) qfChangedF) (Set.toList $ types (Proxy @c))

newtype ChangedR c e = ChangedR e

instance (BundleTypes c) => IntoQueryFilter (ChangedR c Entity) where
  intoQueryFilter (ChangedR e) = and' $ map (\x -> QFWith (x, Nothing) `QFAnd` QFChanged (x, Just e) qfChangedF) (Set.toList $ types (Proxy @c))

instance (BundleTypes c) => IntoQueryFilter (ChangedR c Any) where
  intoQueryFilter _ = and' $ map (\x -> QFWith (x, Nothing) `QFAnd` QFChangedRelAny x qfChangedF) (Set.toList $ types (Proxy @c))

data Added c = Added

instance (BundleTypes c) => IntoQueryFilter (Added c) where
  intoQueryFilter _ = and' $ map (\x -> QFWith (x, Nothing) `QFAnd` QFChanged (x, Nothing) qfAddedF) (Set.toList $ types (Proxy @c))

newtype AddedR c e = AddedR e

instance (BundleTypes c) => IntoQueryFilter (AddedR c Entity) where
  intoQueryFilter (AddedR e) = and' $ map (\x -> QFWith (x, Nothing) `QFAnd` QFChanged (x, Just e) qfAddedF) (Set.toList $ types (Proxy @c))

instance (BundleTypes c) => IntoQueryFilter (AddedR c Any) where
  intoQueryFilter _ = and' $ map (\x -> QFWith (x, Nothing) `QFAnd` QFChangedRelAny x qfAddedF) (Set.toList $ types (Proxy @c))

newtype Check c = Check (c -> Bool)

instance (Component c) => IntoQueryFilter (Check c) where
  intoQueryFilter (Check f) = QFWith (typeRep $ Proxy @c, Nothing) `QFAnd` QFCheckRaw (typeRep $ Proxy @c, ErasedCheck f)

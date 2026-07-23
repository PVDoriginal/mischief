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
  | QFCheckRaw (TypeRep, Maybe Entity, ErasedCheck)
  | QFCheckRawRelAny (TypeRep, ErasedCheck)
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
extractArchetypeFilters (QFCheckRawRelAny x) = (QFCheckRawRelAny x, NoFilter)
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
isArchetypeFilter (QFCheckRawRelAny _) = False
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

newtype With c = With c

instance (Collectable c FilterType) => IntoQueryFilter (With c) where
  intoQueryFilter (With c) =
    let l :: FilterType = collect c
     in and' $ map withF l.inner

newtype Without c = Without c

instance (Collectable c FilterType) => IntoQueryFilter (Without c) where
  intoQueryFilter (Without c) =
    let l :: FilterType = collect c
     in and' $ map (QFNot . withF) l.inner

newtype Not c = Not c

instance (IntoQueryFilter q) => IntoQueryFilter (Not q) where
  intoQueryFilter (Not a) = QFNot $ intoQueryFilter a

data Or a b = Or a b

(|.) :: a -> b -> Or a b
(|.) = Or

data And a b = And a b

(&.) :: a -> b -> And a b
(&.) = And

infix 8 &.

infix 9 |.

instance (IntoQueryFilter a, IntoQueryFilter b) => IntoQueryFilter (a `Or` b) where
  intoQueryFilter (Or a b) = QFOr (intoQueryFilter a) (intoQueryFilter b)

instance (IntoQueryFilter a, IntoQueryFilter b) => IntoQueryFilter (a `And` b) where
  intoQueryFilter (And a b) = QFAnd (intoQueryFilter a) (intoQueryFilter b)

newtype Changed c = Changed c

instance (Collectable c FilterType) => IntoQueryFilter (Changed c) where
  intoQueryFilter (Changed c) =
    let l :: FilterType = collect c
     in and' $ map changedF l.inner

newtype Added c = Added c

instance (Collectable c FilterType) => IntoQueryFilter (Added c) where
  intoQueryFilter (Added c) =
    let l :: FilterType = collect c
     in and' $ map addedF l.inner

newtype Check c = Check (c -> Bool)

instance (Component c) => IntoQueryFilter (Check c) where
  intoQueryFilter (Check f) = checkF (typeRep $ Proxy @c, Nothing, Nothing, ErasedCheck f)

data CheckR e c = CheckR e (c -> Bool)

instance (Component c) => IntoQueryFilter (CheckR Entity c) where
  intoQueryFilter (CheckR e f) = checkF (typeRep $ Proxy @c, Just e, Nothing, ErasedCheck f)

instance (Component c) => IntoQueryFilter (CheckR Any c) where
  intoQueryFilter (CheckR _ f) = checkF (typeRep $ Proxy @c, Nothing, Just Any, ErasedCheck f)

withF :: (TypeRep, Maybe Entity, Maybe Any) -> QueryFilter
withF (c, e, Nothing) = QFWith (c, e)
withF (c, _, _) = QFWithRelAny c

addedF :: (TypeRep, Maybe Entity, Maybe Any) -> QueryFilter
addedF (c, e, Nothing) = QFWith (c, e) `QFAnd` QFChanged (c, e) qfAddedF
addedF (c, _, _) = QFWithRelAny c `QFAnd` QFChangedRelAny c qfAddedF

changedF :: (TypeRep, Maybe Entity, Maybe Any) -> QueryFilter
changedF (c, e, Nothing) = QFWith (c, e) `QFAnd` QFChanged (c, e) qfChangedF
changedF (c, _, _) = QFWithRelAny c `QFAnd` QFChangedRelAny c qfChangedF

checkF :: (TypeRep, Maybe Entity, Maybe Any, ErasedCheck) -> QueryFilter
checkF (c, e, Nothing, f) = QFWith (c, e) `QFAnd` QFCheckRaw (c, e, f)
checkF (c, _, _, f) = QFWithRelAny c `QFAnd` QFCheckRawRelAny (c, f)

newtype FilterType = FilterType {inner :: [(TypeRep, Maybe Entity, Maybe Any)]} deriving newtype (Semigroup)

instance (Component c) => EraseIntoStorage (C c) FilterType where
  erase _ = FilterType [(typeRep $ Proxy @c, Nothing, Nothing)]

instance (Component c) => EraseIntoStorage (R c Entity) FilterType where
  erase (R e) = FilterType [(typeRep $ Proxy @c, Just e, Nothing)]

instance (Component c) => EraseIntoStorage (R c Any) FilterType where
  erase _ = FilterType [(typeRep $ Proxy @c, Nothing, Just Any)]

newtype CheckFilterType = CheckFilterType {inner :: [(TypeRep, Maybe Entity, Maybe Any, ErasedCheck)]} deriving newtype (Semigroup)

instance (Component c) => EraseIntoStorage (C c, c -> Bool) CheckFilterType where
  erase (_, f) = CheckFilterType [(typeRep $ Proxy @c, Nothing, Nothing, ErasedCheck f)]

instance (Component c) => EraseIntoStorage (R c Entity, c -> Bool) CheckFilterType where
  erase (R e, f) = CheckFilterType [(typeRep $ Proxy @c, Just e, Nothing, ErasedCheck f)]

instance (Component c) => EraseIntoStorage (R c Any, c -> Bool) CheckFilterType where
  erase (_, f) = CheckFilterType [(typeRep $ Proxy @c, Nothing, Just Any, ErasedCheck f)]

type family InnerC a where
  InnerC (C (Rel a)) = a
  InnerC (C a) = a

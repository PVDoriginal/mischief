{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MischiefECS.World.Query where

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable hiding (and)
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import {-# SOURCE #-} MischiefECS.App.Systems
import {-# SOURCE #-} MischiefECS.Archetypes.Graph
import MischiefECS.Components
import MischiefECS.Components.BundleTypes
import {-# SOURCE #-} MischiefECS.Components.Spawn
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query.Queryable
import MischiefECS.World.Utils
import Prelude hiding (and)

runQuery :: forall qd m w output. (Queryable qd output, MonadSystem w m) => Proxy qd -> QueryFilter -> World -> m [output]
runQuery query filter world =
  do
    components <-
      liftIO $
        mapM
          ( \(c, t) -> do
              c <- getComponentId c world.components
              return $
                fmap
                  ( \c ->
                      case t of
                        CompQ -> (c, ComponentQuery)
                        RelQ -> (c, RelationshipQueryAny)
                  )
                  c
          )
          (Set.toList (queryTypes query))
    archetypes <- findMatchingArchetypes (catMaybes components) world.archetypes
    let (otherFilter, archetypeFilter) = extractArchetypeFilters filter

    archetypes' <- filterM (\(components, _) -> liftIO $ filterArchetype archetypeFilter components world) archetypes

    outputs <- liftIO $ runQueryInternal query (map snd archetypes') world
    f <- liftIO $ filterQuery @qd world otherFilter (map snd archetypes')
    outputs' <- liftIO $ filterM f (zip [0 ..] outputs)

    return $ map (snd . snd) outputs'

query :: forall qd output m w. (Queryable qd output, MonadSystem w m) => m [output]
query = do
  world <- unsafeGetWorld
  runQuery (Proxy @qd) NoFilter world

entityQuery :: forall qd output m w. (Queryable qd output, MonadSystem w m) => Entity -> m (Maybe output)
entityQuery entity = do
  world <- unsafeGetWorld
  liftIO $ runQueryEntity (Proxy @qd) world entity

get :: forall qd m w out. (Queryable qd out, MonadSystem w m) => Entity -> m (Maybe out)
get = entityQuery @qd

query' :: forall qd m w out. (Queryable qd out, MonadSystem w m) => QueryFilter -> m [out]
query' filter = do
  world <- unsafeGetWorld
  runQuery (Proxy @qd) filter world

single :: forall qd m w out. (Queryable qd out, MonadSystem w m) => m (Maybe out)
single = do
  res <- query @qd
  case res of
    [x] -> return $ Just x
    _ -> return Nothing

single' :: forall qd m w out. (Queryable qd out, MonadSystem w m) => QueryFilter -> m (Maybe out)
single' filter = do
  res <- query' @qd filter
  case res of
    [x] -> return $ Just x
    _ -> return Nothing

res :: forall c out. (Queryable c out, Component c) => System (Maybe out)
res = do
  meta <- meta @c
  get @c meta

-- iter :: forall qd m w. (Queryable qd, MonadSystem w m) => (QueryOutput qd -> m ()) -> m ()
-- iter system = do
--   res <- query @qd
--   for_ res system

-- iter' :: forall qd m w. (Queryable qd, MonadSystem w m) => QueryFilter -> (QueryOutput qd -> m ()) -> m ()
-- iter' filter system = do
--   res <- query' @qd filter
--   for_ res system

-- parIter :: forall qd m w. (Queryable qd, MonadSystem w m) => (QueryOutput qd -> ParSystem ()) -> m ()
-- parIter system = do
--   res <- query @qd
--   parIterList res $ \chunk -> for_ chunk system

filterQuery :: forall qd out. (Queryable qd out) => World -> QueryFilter -> [ArchetypeId] -> IO ((Int, (Entity, out)) -> IO Bool)
filterQuery _ NoFilter _ = return $ const $ return True
filterQuery world (With x) _ = do
  component <- getComponentId x world.components
  case component of
    Nothing -> return (const $ return False)
    Just component ->
      return $
        \(_, (entity, _)) -> do
          components <- findComponentsOfEntity world entity
          case components of
            Nothing -> return True
            Just components ->
              return $ component `elem` components
filterQuery world (WithRel x e) _ = do
  component <- liftIO $ getComponentId x world.components
  case component of
    Nothing -> return (const $ return False)
    Just (ComponentId {id}) -> do
      let component = ComponentId {id, entity = Just e}
      return $
        \(_, (entity, _)) -> do
          components <- findComponentsOfEntity world entity
          case components of
            Nothing -> return True
            Just components ->
              return $ component `elem` components
filterQuery world (Without x) _ = do
  component <- liftIO $ getComponentId x world.components
  case component of
    Nothing -> return (const $ return True)
    Just component ->
      return $
        \(_, (entity, _)) -> do
          components <- findComponentsOfEntity world entity
          case components of
            Nothing -> return True
            Just components ->
              return $ component `notElem` components
filterQuery world (Changed x) archetypes = do
  res <- liftIO $ tryGetTicks x world archetypes
  (lastSystemTick, currentSystemTick) <- getSystemTicks world
  return $
    \(index, _) ->
      let res' = res !! index
       in pure $ res'.changed >= lastSystemTick && res'.changed < currentSystemTick
filterQuery world (Added x) archetypes = do
  res <- liftIO $ tryGetTicks x world archetypes
  (lastSystemTick, currentSystemTick) <- getSystemTicks world
  return $
    \(index, _) ->
      let res' = res !! index
       in pure $ res'.added >= lastSystemTick && res'.added < currentSystemTick
filterQuery world (CheckRaw (x, ef)) archetypes = do
  id <- liftIO $ getComponentId x world.components
  case id of
    Nothing -> return $ const $ return False
    Just id ->
      filterCheck @qd world archetypes id ef
filterQuery world (a `And` b) archetypes = do
  res <- filterQuery @qd world a archetypes
  res2 <- filterQuery @qd world b archetypes
  return $
    \x -> do
      b1 <- res x
      b2 <- res2 x
      return $ b1 && b2
filterQuery world (a `Or` b) archetypes = do
  res <- filterQuery @qd world a archetypes
  res2 <- filterQuery @qd world b archetypes
  return $
    \x -> do
      b1 <- res x
      b2 <- res2 x
      return $ b1 || b2

filterCheck :: forall qd out. (Queryable qd out) => World -> [ArchetypeId] -> ComponentId -> ErasedCheck -> IO ((Int, (Entity, out)) -> IO Bool)
filterCheck world archetypes id (ErasedCheck (f :: (c -> Bool))) = do
  components <- tryGetComponentsFromTables @c world.tables archetypes id
  return $
    \(index, _) ->
      pure $ f (value . snd $ components !! index)

findComponentsOfEntity :: World -> Entity -> IO (Maybe [ComponentId])
findComponentsOfEntity world entity = do
  pointer <- liftIO $ getPointer entity world.entities

  let Tables t = world.tables
  tables <- liftIO $ readIORef t

  case pointer of
    Nothing -> return Nothing
    Just x -> do
      pointer <- liftIO $ readIORef x

      return $ case Map.lookup pointer.archetypeId tables of
        Nothing -> Nothing
        Just x -> Just x.components

data QueryFilter
  = NoFilter
  | With TypeRep
  | WithRel TypeRep Entity
  | Without TypeRep
  | And QueryFilter QueryFilter
  | Or QueryFilter QueryFilter
  | Changed TypeRep
  | Added TypeRep
  | CheckRaw (TypeRep, ErasedCheck)
  deriving (Show)

data ErasedCheck where
  ErasedCheck :: (Component c) => (c -> Bool) -> ErasedCheck

instance Show ErasedCheck where
  show _ = "erased check"

with :: forall qd. (BundleTypes qd) => QueryFilter
with = and' $ map With (Set.toList $ types (Proxy @qd))

withRel :: forall c. (Component c) => Entity -> QueryFilter
withRel = WithRel (typeRep $ Proxy @c)

without :: forall qd. (BundleTypes qd) => QueryFilter
without = and' $ map Without (Set.toList $ types (Proxy @qd))

changed :: forall qd. (BundleTypes qd) => QueryFilter
changed = and' $ map Changed (Set.toList $ types (Proxy @qd))

check :: forall c. (Component c) => (c -> Bool) -> QueryFilter
check f = With (typeRep $ Proxy @c) &. CheckRaw (typeRep $ Proxy @c, ErasedCheck f)

eq :: forall c. (Component c, Eq c) => c -> QueryFilter
eq c = check @c (== c)

added :: forall qd. (BundleTypes qd) => QueryFilter
added = and' $ map Added (Set.toList $ types (Proxy @qd))

and' :: [QueryFilter] -> QueryFilter
and' = foldr (&.) NoFilter

(&.) :: QueryFilter -> QueryFilter -> QueryFilter
(&.) = And

(|.) :: QueryFilter -> QueryFilter -> QueryFilter
(|.) = Or

filterArchetype :: QueryFilter -> [ComponentId] -> World -> IO Bool
filterArchetype NoFilter _ _ = return True
filterArchetype (With x) components world = do
  component <- getComponentId x world.components
  return $ case component of
    Nothing -> False
    Just component -> component `elem` components
filterArchetype (WithRel x e) components world = do
  component <- getComponentId x world.components
  case component of
    Nothing -> pure False
    Just (ComponentId {id}) -> do
      let component = ComponentId {id, entity = Just e}
      return $ component `elem` components
filterArchetype (Without x) components world = do
  component <- getComponentId x world.components
  return $ case component of
    Nothing -> True
    Just component -> component `notElem` components
filterArchetype (a `And` b) c w = do
  x <- filterArchetype a c w
  y <- filterArchetype b c w
  return $ x && y
filterArchetype (a `Or` b) c w = do
  x <- filterArchetype a c w
  y <- filterArchetype b c w
  return $ x || y
filterArchetype _ _ _ = pure True

-- | Extracts archetype-level filters from the bigger filter where possible, to be applied at the start of querying for better performance.
extractArchetypeFilters :: QueryFilter -> (QueryFilter, QueryFilter)
extractArchetypeFilters NoFilter = (NoFilter, NoFilter)
extractArchetypeFilters (With x) = (NoFilter, With x)
extractArchetypeFilters (WithRel x e) = (NoFilter, WithRel x e)
extractArchetypeFilters (Changed x) = (Changed x, NoFilter)
extractArchetypeFilters (Added x) = (Added x, NoFilter)
extractArchetypeFilters (CheckRaw x) = (CheckRaw x, NoFilter)
extractArchetypeFilters (Without x) = (NoFilter, Without x)
extractArchetypeFilters (a `And` b) = (filter1 `And` filter2, res1 `And` res2)
  where
    (filter1, res1) = extractArchetypeFilters a
    (filter2, res2) = extractArchetypeFilters b
extractArchetypeFilters (a `Or` b) =
  if isArchetypeFilter a && isArchetypeFilter b
    then
      (filter1 `Or` filter2, res1 `Or` res2)
    else
      (a `Or` b, NoFilter)
  where
    (filter1, res1) = extractArchetypeFilters a
    (filter2, res2) = extractArchetypeFilters b

isArchetypeFilter :: QueryFilter -> Bool
isArchetypeFilter NoFilter = True
isArchetypeFilter (Changed _) = False
isArchetypeFilter (Added _) = False
isArchetypeFilter (With _) = True
isArchetypeFilter (WithRel _ _) = True
isArchetypeFilter (Without _) = True
isArchetypeFilter (a `And` b) = isArchetypeFilter a || isArchetypeFilter b
isArchetypeFilter (a `Or` b) = isArchetypeFilter a && isArchetypeFilter b
isArchetypeFilter (CheckRaw _) = False

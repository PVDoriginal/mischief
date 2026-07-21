{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.World.Query where

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable hiding (and)
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Mischief.ECS.App.SystemDef
import {-# SOURCE #-} Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Components.Common
import {-# SOURCE #-} Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Query.Val
import Mischief.ECS.World.Utils
import Prelude hiding (and)

runQuery :: forall qd m w output. (Queryable qd output, MonadSystem w m) => qd -> QueryFilter -> World -> m [output]
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
                        RelQ' entity -> (c {entity = Just entity}, RelationshipQuery)
                  )
                  c
          )
          (Set.toList (queryTypes query))
    archetypes <- findMatchingArchetypes (catMaybes components) world.archetypes
    let (otherFilter, archetypeFilter) = extractArchetypeFilters $ preprocessFilter filter

    archetypes' <- filterM (\(components, _) -> liftIO $ (filterArchetype . preprocessFilter) archetypeFilter components world) archetypes

    outputs <- liftIO $ runQueryInternal query (map snd archetypes') world
    f <- liftIO $ filterQuery @qd world (preprocessFilter otherFilter) (map snd archetypes')
    outputs' <- liftIO $ filterM f (zip [0 ..] outputs)

    return $ map (snd . snd) outputs'

query :: forall qd output m w. (Queryable qd output, MonadSystem w m) => qd -> m [output]
query qd = do
  world <- unsafeGetWorld
  runQuery qd NoFilter world

entityQuery :: forall qd output m w. (Queryable qd output, MonadSystem w m) => qd -> Entity -> m (Maybe output)
entityQuery qd entity = do
  world <- unsafeGetWorld
  liftIO $ runQueryEntity qd world entity

get :: forall qd m w out. (Queryable qd out, MonadSystem w m) => qd -> Entity -> m (Maybe out)
get = entityQuery

query' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> m [out]
query' qd filter = do
  world <- unsafeGetWorld
  runQuery qd (collect filter) world

single :: forall qd m w out. (Queryable qd out, MonadSystem w m) => qd -> m (Maybe out)
single qd = do
  res <- query qd
  case res of
    [x] -> return $ Just x
    _ -> return Nothing

single' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> m (Maybe out)
single' qd filter = do
  res <- query' qd filter
  case res of
    [x] -> return $ Just x
    _ -> return Nothing

res :: forall c. (Queryable (C c) (Result c), Component c) => System (Maybe (Result c))
res = do
  meta <- meta @c
  get (C @c) meta

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
filterQuery world (QFWith (x, entity)) _ = do
  component <- fmap (\x -> x {entity}) <$> getComponentId x world.components
  case component of
    Nothing -> return (const $ return False)
    Just component ->
      return $
        \(_, (entity, _)) -> do
          components <- findComponentsOfEntity world entity
          case components of
            Nothing -> return False
            Just components ->
              return $ component `elem` components
filterQuery world (QFWithRelAny x) _ = do
  component <- liftIO $ getComponentId x world.components
  case component of
    Nothing -> return (const $ return False)
    Just (ComponentId {id}) -> do
      return $
        \(_, (entity, _)) -> do
          components <- findComponentsOfEntity world entity
          case components of
            Nothing -> return False
            Just components ->
              return $ any (\c -> isJust c.entity && c.id == id) components
filterQuery world (QFChanged (x, entity) f) archetypes = do
  component <- fmap (\x -> x {entity}) <$> getComponentId x world.components
  case component of
    Nothing -> return $ const $ pure False
    Just component -> do
      res <- liftIO $ tryGetTicks component world archetypes
      (lastSystemTick, currentSystemTick) <- getSystemTicksInternal world
      return $
        \(index, _) -> pure $ case res !! index of
          Nothing -> False
          Just res' -> f res' lastSystemTick currentSystemTick
filterQuery world (QFChangedRelAny x f) _ = do
  component <- liftIO $ getComponentId x world.components
  case component of
    Nothing -> return (const $ return False)
    Just (ComponentId {id}) -> do
      return $
        \(_, (entity, _)) -> do
          components <- findComponentsOfEntity world entity
          case components of
            Nothing -> return True
            Just components' -> do
              let components = filter (\c -> isJust c.entity && c.id == id) components'
              ticks <- catMaybes <$> mapM (\x -> tryGetEntityTicks entity x world) components
              (lastSystemTick, currentSystemTick) <- getSystemTicksInternal world
              return $ any (\t -> f t lastSystemTick currentSystemTick) ticks
filterQuery world (QFCheckRaw (x, entity, ef)) archetypes = do
  id <- fmap (\a -> a {entity}) <$> getComponentId x world.components
  case id of
    Nothing -> return $ const $ return False
    Just id ->
      filterCheck @qd world archetypes id ef
filterQuery world (QFCheckRawRelAny (_, ef)) archetypes = filterCheckRelAny @qd world archetypes ef
filterQuery world (a `QFAnd` b) archetypes = do
  res <- filterQuery @qd world a archetypes
  res2 <- filterQuery @qd world b archetypes
  return $
    \x -> do
      b1 <- res x
      b2 <- res2 x
      return $ b1 && b2
filterQuery world (a `QFOr` b) archetypes = do
  res <- filterQuery @qd world a archetypes
  res2 <- filterQuery @qd world b archetypes
  return $
    \x -> do
      b1 <- res x
      b2 <- res2 x
      return $ b1 || b2
filterQuery world (QFNot a) archetypes = do
  f <- filterQuery @qd world a archetypes
  return $ \x -> do
    r <- f x
    return $ not r

filterCheckRelAny :: forall qd out. (Queryable qd out) => World -> [ArchetypeId] -> ErasedCheck -> IO ((Int, (Entity, out)) -> IO Bool)
filterCheckRelAny world archetypes (ErasedCheck (f :: (c -> Bool))) = do
  components <- tryGetRelCollections @c world archetypes

  return $
    \(index, _) -> do
      pure $ any (f . (\x -> x.comp)) (snd $ components !! index)

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

class GetResultComponentId c where
  getResultComponentId :: (MonadSystem w m) => c -> m (Maybe ComponentId)

class GetResultComponentId' flag c where
  getResultComponentId' :: (MonadSystem w m) => c -> m (Maybe ComponentId)

instance (Component c) => GetResultComponentId' True (Result c) where
  getResultComponentId' _ = fmap (`ComponentId` Nothing) <$> tryMeta @c

instance (Component c) => GetResultComponentId' False (Result (Rel c)) where
  getResultComponentId' r = fmap (`ComponentId` Just r.target) <$> tryMeta @c

instance (GetResultComponentId' (IsComp c) (Result c)) => GetResultComponentId (Result c) where
  getResultComponentId = getResultComponentId' @(IsComp c)

addedChanged :: forall c m w. (MonadSystem w m, GetResultComponentId (Result c)) => (ComponentTicks -> Tick -> Tick -> Bool) -> Result c -> m Bool
addedChanged f r = do
  id <- getResultComponentId r
  case id of
    Nothing -> return False
    Just id -> do
      world <- unsafeGetWorld
      ticks <- liftIO $ tryGetEntityTicks (entityOf r) id world
      case ticks of
        Nothing -> return False
        Just ticks -> do
          (lastSystemTick, currentSystemTick) <- liftIO $ getSystemTicksInternal world
          return $ f ticks lastSystemTick currentSystemTick

added :: forall c m w. (MonadSystem w m, GetResultComponentId (Result c)) => Result c -> m Bool
added = addedChanged qfAddedF

changed :: forall c m w. (MonadSystem w m, GetResultComponentId (Result c)) => Result c -> m Bool
changed = addedChanged qfChangedF

getSystemTicksInternal :: World -> IO (Tick, Tick)
getSystemTicksInternal world = do
  let (SystemId sys) = world.systemId
  runSystem
    ( do
        Just (a, b) <- get (C @LastSystemTick, C @SystemTick) sys
        return (a.inner, b.inner)
    )
    world
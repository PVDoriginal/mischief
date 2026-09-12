{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.World.Query where

import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable
import Data.Foldable hiding (and)
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Base (Int (..), eqWord#, isTrue#)
import Mischief.ECS.App.SystemDef
import Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Components.Common
import Mischief.ECS.Entities
import Mischief.ECS.EntityDef
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.Vec qualified as Vec
import Mischief.ECS.World
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Utils
import Prelude hiding (and)

findArchetypes :: forall qd m w output. (Queryable qd output, MonadSystem w m) => qd -> m [([ComponentId], ArchetypeId)]
findArchetypes query = do
  world <- unsafeGetWorld
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
                      RelQ' entity -> (setCompIdTarget (Just entity) c, RelationshipQuery)
                )
                c
        )
        (Set.toList (queryTypes query))

  findMatchingArchetypes (catMaybes components) world.archetypes

runQuery :: forall qd m w output a. (Queryable qd output, MonadSystem w m) => qd -> QueryFilter -> World -> m [output]
runQuery query filter world =
  do
    archetypes <- findArchetypes query
    -- let (otherFilter, archetypeFilter) = extractArchetypeFilters $ preprocessFilter filter

    -- archetypes' <- filterM (\(components, _) -> liftIO $ (filterArchetype . preprocessFilter) archetypeFilter components world) archetypes

    -- outputs <- liftIO $ runQueryInternal query (map snd archetypes') world
    -- outputs' <- filterM (\(e, b, _) -> (&& b) <$> filterQuery (preprocessFilter otherFilter) e) outputs
    -- return $ map (\(_, _, o) -> o) outputs'
    undefined

-- query :: forall qd output m w. (Queryable qd output, MonadSystem w m) => qd -> m [output]
-- query qd = do
--   world <- unsafeGetWorld
--   runQuery qd NoFilter world

entityQuery :: forall qd output m w. (Queryable qd output, MonadSystem w m) => qd -> Entity -> m (Maybe output)
entityQuery qd entity = do
  world <- unsafeGetWorld
  liftIO $ runQueryEntity qd world entity

-- get :: forall qd m w out. (Queryable qd out, MonadSystem w m) => qd -> Entity -> m (Maybe out)
-- get = entityQuery

-- get' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> Entity -> m (Maybe out)
-- get' qd qf entity = do
--   b <- filterQuery (preprocessFilter $ collect qf) entity
--   if b
--     then
--       entityQuery qd entity
--     else
--       pure Nothing

-- query' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> m [out]
-- query' qd filter = do
--   world <- unsafeGetWorld
--   runQuery qd (collect filter) world

-- single :: forall qd m w out. (Queryable qd out, MonadSystem w m) => qd -> m (Maybe out)
-- single qd = do
--   res <- query qd
--   case res of
--     [x] -> return $ Just x
--     _ -> return Nothing

-- single' :: forall qd m w out qf. (Queryable qd out, MonadSystem w m, Collectable qf QueryFilter) => qd -> qf -> m (Maybe out)
-- single' qd filter = do
--   res <- query' qd filter
--   case res of
--     [x] -> return $ Just x
--     _ -> return Nothing

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

class GetResultComponentId c where
  getResultComponentId :: (MonadSystem w m) => c -> m (Maybe ComponentId)

class GetResultComponentId' flag c where
  getResultComponentId' :: (MonadSystem w m) => c -> m (Maybe ComponentId)

-- instance (Component c) => GetResultComponentId' True (Result c) where
--   getResultComponentId' _ = fmap (\(Entity (# id, _ #)) -> ComponentId (# id, Nothing #)) <$> tryMetaLocal @c

-- instance (Component c) => GetResultComponentId' False (Result (Rel c)) where
--   getResultComponentId' r = fmap (\(Entity (# id, _ #)) -> ComponentId (# id, Just r.target #)) <$> tryMetaLocal @c

tryMetaLocal :: forall c m w. (Component c, MonadSystem w m) => m (Maybe Entity)
tryMetaLocal = do
  world <- unsafeGetWorld
  component <- liftIO $ getComponentId (typeRep $ Proxy @c) world.components
  return $ fmap (\(ComponentId (# id, _ #)) -> Entity (# id, 0## #)) component

-- instance (GetResultComponentId' (IsComp c) (Result c)) => GetResultComponentId (Result c) where
--   getResultComponentId = getResultComponentId' @(IsComp c)

-- addedChanged :: forall c m w. (MonadSystem w m, GetResultComponentId (Result c)) => (ComponentTicks -> Tick -> Tick -> Bool) -> Result c -> m Bool
-- addedChanged f r = do
--   id <- getResultComponentId r
--   case id of
--     Nothing -> return False
--     Just id -> do
--       world <- unsafeGetWorld
--       ticks <- liftIO $ tryGetEntityTicks (entityOf r) id world
--       case ticks of
--         Nothing -> return False
--         Just ticks -> do
--           (lastSystemTick, currentSystemTick) <- liftIO $ getSystemTicksInternal world
--           return $ f ticks lastSystemTick currentSystemTick

-- added :: forall c m w. (MonadSystem w m, GetResultComponentId (Result c)) => Result c -> m Bool
-- added = addedChanged qfAddedF

-- changed :: forall c m w. (MonadSystem w m, GetResultComponentId (Result c)) => Result c -> m Bool
-- changed = addedChanged qfChangedF

-- newtype Query a = Query [(Entity, a)]

data Query a where
  BuildQuery :: (Queryable qd out) => qd -> QueryFilter -> Query out
  ChangeQuery :: Query b -> ([(Entity, b)] -> System [(Entity, a)]) -> Query a

-- qread :: Query out -> [out]
-- qread (Query a) = map snd a

-- get :: (Queryable qd out) => qd -> Entity -> System (Maybe out)
-- get = entityQuery

-- get' :: (Queryable qd out) => (qd, QueryFilter ArchetypeFilter) -> Entity -> System (Maybe out)
-- get' (qd, qf) entity = do
--   b <- filterQuery qf entity
--   if b then entityQuery qd entity else pure Nothing

-- query :: (Queryable qd out) => qd -> System (Query out)
-- query qd = query' (qd, NoFilter)

-- query' :: (Queryable qd out) => (qd, QueryFilter ArchetypeFilter) -> Query out
-- query' (qd, qf) = do
--   world <- unsafeGetWorld
--   archetypes' <- findArchetypes qd
--   archetypes <- liftIO (filterM (filterArchetype qf world . fst) archetypes')
--   x <- liftIO $ runQueryInternal (E, qd) (map snd archetypes) world

--   pure $   $ mapMaybe (\case (_, False, _) -> Nothing; (_, True, x) -> Just x) x

get :: Entity -> Query out -> System (Maybe out)
get entity (BuildQuery qd qf) = do
  world <- unsafeGetWorld
  b <- liftIO $ filterEntity qf world entity
  if b then entityQuery qd entity else pure Nothing
get entity (ChangeQuery a f) = do
  x <- get entity a
  case x of
    Nothing -> pure Nothing
    Just x -> do
      [(_, y)] <- f [(entity, x)]
      pure $ Just y

qrun' :: Query out -> System [(Entity, out)]
qrun' (BuildQuery qd qf) = do
  world <- unsafeGetWorld
  archetypes' <- findArchetypes qd
  archetypes <- liftIO (filterM (filterArchetype qf world . fst) archetypes')
  x <- liftIO $ runQueryInternal (E, qd) (map snd archetypes) world
  pure $ mapMaybe (\case (_, False, _) -> Nothing; (_, True, x) -> Just x) x
qrun' (ChangeQuery a f) = f =<< qrun' a

query :: Query out -> System [out]
query a = map snd <$> qrun' a

qrun :: Query out -> System ()
qrun a = void $ qrun' a

mkQuery :: (Queryable qd out) => qd -> Query out
mkQuery x = mkQuery' x NoFilter

mkQuery' :: (Queryable qd out) => qd -> QueryFilter -> Query out
mkQuery' = BuildQuery
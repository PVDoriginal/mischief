{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Remove (remove, delete, removeRel, triggerRemoveEvent) where

import Control.Monad
import Control.Monad.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Mischief.ECS.Archetypes
import {-# SOURCE #-} Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.Common
import {-# SOURCE #-} Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Change (changeArchetype)
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Utils

class Removable c where
  getTypes :: Proxy c -> System (Set ComponentId)

instance {-# OVERLAPPABLE #-} (Component c) => Removable c where
  getTypes c = do
    x <- getOrAddComponentId (ComponentType c)
    return $ Set.singleton x

newtype ToRemove = ToRemove {inner :: [(ComponentType, Maybe Entity, Maybe Any)]} deriving newtype (Semigroup)

instance (Component c) => EraseIntoStorage (C c) ToRemove where
  erase _ = ToRemove [(ComponentType $ Proxy @c, Nothing, Nothing)]

instance (Component c) => EraseIntoStorage (R c Entity) ToRemove where
  erase (R e) = ToRemove [(ComponentType $ Proxy @c, Just e, Nothing)]

instance (Component c) => EraseIntoStorage (R c Any) ToRemove where
  erase _ = ToRemove [(ComponentType $ Proxy @c, Nothing, Just Any)]

-- remove :: forall r. (Removable r) => Entity -> System ()
-- remove entity = do
--   types <- getTypes (Proxy @r)
--   removeFromEntity (Set.toList types) entity

class Delete r where
  delete :: r -> System ()

class Delete' r isRel where
  delete' :: r -> System ()

instance (Delete' (Result r) (IsRel r)) => Delete (Result r) where
  delete = delete' @(Result r) @(IsRel r)

instance (Component c) => Delete' (Result c) False where
  delete' :: Result c -> System ()
  delete' result = remove (C @c) (entityOf result)

instance (Component c) => Delete' (Result (Rel c)) True where
  delete' :: Result (Rel c) -> System ()
  delete' result = remove (R @c result.target) (entityOf result)

remove :: (Collectable c ToRemove) => c -> Entity -> System ()
remove c entity = do
  let list :: ToRemove = collect c
  for_ list.inner $ \case
    (x, Nothing, Nothing) -> do
      comp <- getOrAddComponentId x
      removeFromEntity [comp] entity
    (x, Just target, _) -> do
      comp <- getOrAddPairId (Pair (x, target))
      removeFromEntity [comp] entity
    (x, _, Just _) -> do
      removeRelationshipsFromEntity x entity

removeRel :: forall c. (Component c) => Entity -> Entity -> System ()
removeRel = removeRelationshipFromEntity @c

removeRelationshipFromEntity :: forall c. (Component c) => Entity -> Entity -> System ()
removeRelationshipFromEntity target entity = do
  componentId <- getOrAddPairId (Pair (ComponentType $ Proxy @c, target))
  removeFromEntity [componentId] entity

removeRelationshipsFromEntity :: ComponentType -> Entity -> System ()
removeRelationshipsFromEntity x entity = do
  world <- unsafeGetWorld
  ids <- liftIO $ findComponentsOfEntity world entity
  comp <- getOrAddComponentId x
  for_ ids $ \ids' -> do
    let ids = filter (\x -> x.id == comp.id) ids'
    removeFromEntity ids entity

removeFromEntity :: [ComponentId] -> Entity -> System ()
removeFromEntity components entity = do
  world <- unsafeGetWorld
  pointer <- liftIO $ getPointer entity world.entities

  case pointer of
    Nothing -> warn $ "Removal failed: Entity " <> text entity <> " is not alive."
    Just pointer -> do
      currentPointer <- liftIO $ readIORef pointer

      (newArchetype, removedComponents) <- getArchetypeOnRemove currentPointer.archetypeId components
      triggerRemoveEvent removedComponents entity

      void $ changeArchetype entity newArchetype Nothing

triggerRemoveEvent :: [ComponentId] -> Entity -> System ()
triggerRemoveEvent components entity = do
  for_ components $ \component -> do
    Just t <- get (C @ComponentType) component.id
    case component.entity of
      Nothing -> triggerRemoveEventC (value t) entity
      Just target -> triggerRemoveEventR (value t) target entity

triggerRemoveEventC :: ComponentType -> Entity -> System ()
triggerRemoveEventC (ComponentType (_ :: Proxy t)) entity =
  runEvent $ eraseEvent $ OnRemove @t entity

triggerRemoveEventR :: ComponentType -> Entity -> Entity -> System ()
triggerRemoveEventR (ComponentType (_ :: Proxy t)) target entity =
  runEvent $ eraseEvent $ OnRemoveRel @t entity target
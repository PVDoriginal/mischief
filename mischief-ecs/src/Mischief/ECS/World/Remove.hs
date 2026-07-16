{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Remove (remove, delete, removeRel, triggerRemoveEvent) where

import Control.Monad
import Control.Monad.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Mischief.ECS.Archetypes
import {-# SOURCE #-} Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Components
import {-# SOURCE #-} Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Change (changeArchetype)
import Mischief.ECS.World.Query
import Mischief.ECS.World.Utils

class Removable c where
  getTypes :: Proxy c -> System (Set ComponentId)

instance {-# OVERLAPPABLE #-} (Component c) => Removable c where
  getTypes c = do
    x <- getOrAddComponentId (ComponentType c)
    return $ Set.singleton x

-- instance {-# OVERLAPPING #-} (Removable c0, Removable c1) => Removable (c0, c1) where
--   getTypes c0 c1 = do
--     world <- ask

remove :: forall r. (Removable r) => Entity -> System ()
remove entity = do
  types <- getTypes (Proxy @r)
  removeFromEntity (Set.toList types) entity

class Delete r where
  delete :: r -> System ()

instance (Component c) => Delete (Result c) where
  delete :: Result c -> System ()
  delete result = remove @c (entityOf result)

instance (Component c) => Delete (RelResult c) where
  delete :: RelResult c -> System ()
  delete result = removeRelationshipFromEntity @c (target result) (entityOf result)

removeRel :: forall c. (Component c) => Entity -> Entity -> System ()
removeRel = removeRelationshipFromEntity @c

removeRelationshipFromEntity :: forall c. (Component c) => Entity -> Entity -> System ()
removeRelationshipFromEntity target entity = do
  componentId <- getOrAddPairId (Pair (ComponentType $ Proxy @c, target))
  removeFromEntity [componentId] entity

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
    Just t <- get @ComponentType component.id
    case component.entity of
      Nothing -> triggerRemoveEventC (value t) entity
      Just target -> triggerRemoveEventR (value t) target entity

triggerRemoveEventC :: ComponentType -> Entity -> System ()
triggerRemoveEventC (ComponentType (_ :: Proxy t)) entity =
  runEvent $ eraseEvent $ OnRemove @t entity

triggerRemoveEventR :: ComponentType -> Entity -> Entity -> System ()
triggerRemoveEventR (ComponentType (_ :: Proxy t)) target entity =
  runEvent $ eraseEvent $ OnRemoveRel @t entity target
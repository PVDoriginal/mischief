{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.World.Remove where

import Control.Monad
import Control.Monad.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map qualified as Map
import MischiefECS.Archetypes
import {-# SOURCE #-} MischiefECS.Archetypes.Graph
import MischiefECS.Components
import {-# SOURCE #-} MischiefECS.Components.Spawn
import MischiefECS.Entities
import MischiefECS.Events
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Change (changeArchetype)
import MischiefECS.World.Utils

class Removable c where
  removeInternal :: Proxy c -> Entity -> System ()

instance {-# OVERLAPPABLE #-} (Component c) => Removable c where
  removeInternal :: Proxy c -> Entity -> System ()
  removeInternal _ entity = do
    runEvent $ eraseEvent (OnRemove @c entity)
    removeComponentFromEntity @c entity

instance {-# OVERLAPPING #-} (Removable c0, Removable c1) => Removable (c0, c1) where
  removeInternal :: Proxy (c0, c1) -> Entity -> System ()
  removeInternal _ !entity = do
    removeInternal (Proxy @c0) entity
    removeInternal (Proxy @c1) entity

remove :: forall r. (Removable r) => Entity -> System ()
remove = removeInternal (Proxy @r)

class Delete r where
  delete :: r -> System ()

instance (Component c) => Delete (ComponentResult c) where
  delete :: ComponentResult c -> System ()
  delete result = remove @c result.entity

instance (Component c) => Delete (RelationshipResult c) where
  delete :: RelationshipResult c -> System ()
  delete result = removeRelationshipFromEntity @c result.target result.entity

instance (Component c) => Delete (RelationshipCollection c) where
  delete :: RelationshipCollection c -> System ()
  delete collection = for_ collection.collection delete

removeComponentFromEntity :: forall c. (Component c) => Entity -> System ()
removeComponentFromEntity entity =
  do
    world <- ask
    componentId <- getOrAddComponentId (ComponentType $ Proxy @c) world.components
    removeFromEntity componentId entity

removeRelationshipFromEntity :: forall c. (Component c) => Entity -> Entity -> System ()
removeRelationshipFromEntity target entity = do
  world <- ask
  componentId <- getOrAddPairId (Pair (ComponentType $ Proxy @c, target)) world.components
  removeFromEntity componentId entity

removeFromEntity :: ComponentId -> Entity -> System ()
removeFromEntity componentId entity = do
  world <- ask
  pointer <- liftIO $ getPointer entity world.entities

  case pointer of
    Nothing -> undefined
    Just pointer -> do
      let Tables tables = world.tables
      tables <- liftIO $ readIORef tables
      currentPointer <- liftIO $ readIORef pointer

      case Map.lookup currentPointer.archetypeId tables of
        Nothing -> undefined
        Just currentTable -> do
          when (componentId `elem` currentTable.components) $ do
            newArchetype <- getArchetypeOnRemove currentPointer.archetypeId [componentId]
            changeArchetype entity newArchetype Nothing

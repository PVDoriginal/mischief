{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.World.Remove (remove, delete) where

import Control.Monad
import Control.Monad.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import MischiefECS.Archetypes
import {-# SOURCE #-} MischiefECS.Archetypes.Graph
import MischiefECS.Components
import {-# SOURCE #-} MischiefECS.Components.Spawn
import MischiefECS.Entities
import MischiefECS.Events
import MischiefECS.Log
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Change (changeArchetype)
import MischiefECS.World.Utils

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

      newArchetype <- getArchetypeOnRemove currentPointer.archetypeId components
      void $ changeArchetype entity newArchetype Nothing

-- triggerRemoveEvent :: ErasedComponent
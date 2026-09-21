{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.World.Remove (remove, removeRel, triggerRemoveEvent) where

import Control.Monad
import Control.Monad.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Text qualified as T
import GHC.Base (Int (..), eqWord#, isTrue#)
import Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.HooksDef (HookContext (..), HookContextRel (..))
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.EventDef
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.World
import Mischief.ECS.World.Change (changeArchetype)
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.Queryable

newtype ToRemove = ToRemove {inner :: [(ComponentType, Maybe Entity, Maybe Any)]} deriving newtype (Semigroup)

instance (Component c) => EraseIntoStorage (C c) ToRemove where
  erase _ = ToRemove [(ComponentType $ Proxy @c, Nothing, Nothing)]

instance (Component c) => EraseIntoStorage (R c Entity) ToRemove where
  erase (R e) = ToRemove [(ComponentType $ Proxy @c, Just e, Nothing)]

instance (Component c) => EraseIntoStorage (R c Any) ToRemove where
  erase _ = ToRemove [(ComponentType $ Proxy @c, Nothing, Just Any)]

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
  (ComponentId (# id, _ #)) <- getOrAddComponentId x
  for_ ids $ \ids' -> do
    let ids = filter (\(ComponentId (# id', _ #)) -> isTrue# $ eqWord# id id') ids'
    removeFromEntity ids entity

removeFromEntity :: [ComponentId] -> Entity -> System ()
removeFromEntity components entity = do
  world <- unsafeGetWorld
  pointer <- liftIO $ getPointer entity world.entities

  case pointer of
    Nothing -> warn $ "Removal failed: Entity " <> T.show entity <> " is not alive."
    Just pointer -> do
      (EntityPointer (# archetypeId, _ #)) <- liftIO $ readIORef pointer

      (newArchetype, removedComponents) <- getArchetypeOnRemove (ArchetypeId $ I# archetypeId) components
      triggerRemoveEvent removedComponents entity

      void $ changeArchetype entity newArchetype Nothing

triggerRemoveEvent :: [ComponentId] -> Entity -> System ()
triggerRemoveEvent components entity = do
  for_ components $ \(ComponentId (# id, target #)) -> do
    Just t <- single $ mkGet (Entity (# id, 0## #)) (C @ComponentType)
    case target of
      Nothing -> triggerRemoveEventC t entity
      Just target -> triggerRemoveEventR t target entity

triggerRemoveEventC :: ComponentType -> Entity -> System ()
triggerRemoveEventC (ComponentType (_ :: Proxy t)) entity = do
  runEvent $ eraseEvent $ OnRemove @t entity

  let context = HookContext {entity}
  m <- meta @t
  Just hooks <- single $ mkGet m (M @ComponentRemoveHooks)
  for_ hooks $ \(ComponentRemoveHooks h) -> do
    for_ h $ \h -> h context

triggerRemoveEventR :: ComponentType -> Entity -> Entity -> System ()
triggerRemoveEventR (ComponentType (_ :: Proxy t)) target entity = do
  runEvent $ eraseEvent $ OnRemoveRel @t entity target

  let context = HookContextRel {entity, target}

  m <- meta @t
  Just hooks <- single $ mkGet m (M @ComponentRemoveHooksRel)
  for_ hooks $ \(ComponentRemoveHooksRel h) -> do
    for_ h $ \h -> h context

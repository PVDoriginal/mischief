{-# LANGUAGE AllowAmbiguousTypes #-}

-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.Hooks
  ( relComplementary,
    relCleanup,
    relCleanupRemove,
    relCleanupDespawn,
  )
where

import Control.Monad
import Data.Data
import Data.Foldable
import Data.Kind
import Data.Maybe
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.Hooks
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.Utils
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Utils

-- | When applied on a component @A@, this hook takes a function @(A -> B)@ and does two things:
--
-- 1. When @'Rel' (A, y)@ is inserted on entity @x@, @'Rel' (B, x)@ will be inserted on @y@ (the value of @B@ obtained through the provided function).
--
-- 2. When @'Rel' (a, y)@ is removed from an entity @x@, @'Rel' (B, x)@ will be removed from @y@.
relComplementary :: forall (a :: Type) b. (Component a, Component b) => (a -> b) -> Hooks a
relComplementary f = collect (insertComplementary f, removeComplementary @b @a)

insertComplementary :: forall (a :: Type) b. (Component b, Component a) => (a -> b) -> OnInsertRel a -> System ()
insertComplementary f event = do
  warn $ text event.target
  Just val <- pickRel event.target . unwrap <$> get (R @a Any) event.entity
  insert (Rel (f val.comp) event.entity) event.target

removeComplementary :: forall b a. (Component b) => OnRemoveRel a -> System ()
removeComplementary event = removeRel @b event.entity event.target

relCleanup :: forall (a :: Type). (Component a) => (CleanupRequest -> System ()) -> Hooks a
relCleanup f = collect (insertCleanupWatcher @a f, removeCleanupWatcher @a)

relCleanupRemove :: forall (a :: Type). (Component a) => Hooks a
relCleanupRemove = relCleanup (\r -> removeRel @a r.target r.entity)

relCleanupDespawn :: forall (a :: Type). (Component a) => Hooks a
relCleanupDespawn = relCleanup (\r -> despawn r.entity)

insertCleanupWatcher :: forall c. (Component c) => (CleanupRequest -> System ()) -> OnInsertRel c -> System ()
insertCleanupWatcher f e = insert (Rel (CleanupWatcher @c f) e.entity) e.target

removeCleanupWatcher :: forall c. (Component c) => OnRemoveRel c -> System ()
removeCleanupWatcher e = do
  insert (Rel (CleanupWatcher @c (pure . pure ())) e.entity) e.target
  removeRel @(CleanupWatcher c) e.entity e.target

newtype CleanupWatcher c = CleanupWatcher {function :: CleanupRequest -> System ()}

data CleanupRequest = CleanupRequest
  { entity :: Entity,
    target :: Entity
  }

instance (Component c) => Component (CleanupWatcher c) where
  hooks = collect $ triggerCleanup @c

triggerCleanup :: forall c. (Component c) => OnRemoveRel (CleanupWatcher c) -> System ()
triggerCleanup e = do
  Just watcher <- pickRel e.target . unwrap <$> get (R @(CleanupWatcher c) Any) e.entity
  watcher.comp.function CleanupRequest {entity = e.target, target = e.entity}

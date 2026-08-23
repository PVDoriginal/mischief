{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.HooksRel
  ( addOther,
    removeOther,
    addDespawnCleaner,
    addRemoveCleaner,
    addCleaner,
    removeCleaner,
  )
where

import Data.Kind
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Components.HooksDef
import Mischief.ECS.EntityDef
import Mischief.ECS.Events
import Mischief.ECS.Hooks
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn

-- | Relationship Hook for @Component a@.
--
-- Takes a @a -> b@. If @a@ is a relationship from @x@ to @y@, this hook will add the corresponding @b@ from @y@ to @x@.
--
-- Example usage:
--
-- @
-- data After = After
-- data Before = Before
--
-- instance 'Component' Before where
--   onAdd = addOther ('const' After)
--
-- instance 'Component' After where
--   onAdd = addOther ('const' Before)
-- @
addOther :: forall (a :: Type) b. (Component a, Component b) => (a -> b) -> HooksRel a
addOther f = hookRel $ insertComplementary f

-- | Relationship Hook for @Component a@.
--
-- Removes @b@ from the target entity of a relationship.
--
-- Example usage:
--
-- @
-- data Likes = Likes
-- data LikedBy = LikedBy deriving (Component)
--
-- instance 'Component' Likes where
--   onAdd = addOther ('const' Likes)
--   onRemove = removeOther \@Likes
-- @`
removeOther :: forall a b. (Component b) => HooksRel a
removeOther = hookRel $ removeComplementary @b

insertComplementary :: forall (a :: Type) b. (Component b, Component a) => (a -> b) -> HookContextRel -> System ()
insertComplementary f event = do
  Just val <- get (R @a event.target) event.entity
  insert (Rel (f val.comp) event.entity) event.target

removeComplementary :: forall b. (Component b) => HookContextRel -> System ()
removeComplementary event = remove (R @b event.entity) event.target

addCleaner :: forall (a :: Type). (Component a) => (CleanupRequest -> System ()) -> HooksRel a
addCleaner f = hookRel $ insertCleanupWatcher @a f

-- | Relationship Hook for @Component a@.
--
-- Adds a cleaner that will remove this relationship if the target is despawned.
--
-- Note that one component can have only one cleaner at a time.
addRemoveCleaner :: forall (a :: Type). (Component a) => HooksRel a
addRemoveCleaner = addCleaner @a (\r -> removeRel @a r.target r.entity)

-- | Relationship Hook for @Component a@.
--
-- Adds a cleaner that will despawn this relationship's entity if the target of the relationship is despawned.
--
-- Note that one component can have only one cleaner at a time.
addDespawnCleaner :: forall (a :: Type). (Component a) => HooksRel a
addDespawnCleaner = addCleaner @a (\r -> despawn r.entity)

insertCleanupWatcher :: forall c. (Component c) => (CleanupRequest -> System ()) -> HookContextRel -> System ()
insertCleanupWatcher f e = insert (Rel (CleanupWatcher @c f) e.entity) e.target

-- | Relationship Hook for @Component a@.
--
-- Removes the cleaner associated to this component, if any.
removeCleaner :: forall (a :: Type). (Component a) => HooksRel a
removeCleaner = hookRel (removeCleanupWatcher @a)

removeCleanupWatcher :: forall c. (Component c) => HookContextRel -> System ()
removeCleanupWatcher e = do
  insert (Rel (CleanupWatcher @c (pure . pure ())) e.entity) e.target
  removeRel @(CleanupWatcher c) e.entity e.target

newtype CleanupWatcher c = CleanupWatcher {function :: CleanupRequest -> System ()}

data CleanupRequest = CleanupRequest
  { entity :: Entity,
    target :: Entity
  }

instance (Component c) => Component (CleanupWatcher c) where
  onRemoveRel = hookRel (triggerCleanup @c)

triggerCleanup :: forall c. (Component c) => HookContextRel -> System ()
triggerCleanup e = do
  Just watcher <- get (R @(CleanupWatcher c) e.target) e.entity
  watcher.comp.function CleanupRequest {entity = e.target, target = e.entity}

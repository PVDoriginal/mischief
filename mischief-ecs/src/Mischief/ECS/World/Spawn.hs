module Mischief.ECS.World.Spawn where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Text qualified as T
import GHC.Base (Int (..))
import GHC.Stack
import Mischief.ECS.Archetypes.Graph (getArchetypeOnSpawn)
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.EventDef
import Mischief.ECS.Log
import Mischief.ECS.Observer
import Mischief.ECS.Tables
import Mischief.ECS.Vec qualified as Vec
import Mischief.ECS.World
import Mischief.ECS.World.Change
import Mischief.ECS.World.Defer
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Prefs
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Utils

-- | Spawn an entity given a bundle of components.
spawn :: (HasCallStack, Bundle b) => b -> System Entity
spawn bundle =
  do
    world <- unsafeGetWorld
    entity <- liftIO $ getNewEntity world.entities

    spawnEntity entity bundle
    return entity

-- | Same as 'spawn' but discards the returned entity.
spawn_ :: (HasCallStack, Bundle b) => b -> System ()
spawn_ = void . spawn

-- | Spawn an entity given a bundle of components, inside a @ParSystem@.
--
-- This will immediately reserve and return an Entity index which can be used, while defering
-- the actual spawn.
spawnDefer :: (Bundle b) => b -> ParSystem Entity
spawnDefer bundle = do
  world <- unsafeGetWorld
  entity <- liftIO $ getNewEntity world.entities

  defer $ spawnEntity entity bundle
  return entity

data SpawnEventsSettings = WithSpawnEvents | WithoutSpawnEvents

-- | Spawn an Entity given an existing, reserved id. This is not meant for general use.
spawnEntity :: (HasCallStack, Bundle b) => Entity -> b -> System ()
spawnEntity entity bundle = do
  world <- unsafeGetWorld
  let BundleData {elements, resources, external} = addComponentToBundleData (Name (show entity)) $ bundleData bundle

  for_ external $ \(e, s) -> do
    insert s e

  for_ resources $ \BundleElement {component = ErasedComponent (val :: c)} -> do
    m <- meta @c
    insert val m

  currentTick <- liftIO $ readIORef world.tick
  bundleD <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

  archetype <- getArchetypeOnSpawn $ map (\x -> x.id) bundleD.elements

  entityPointer <- liftIO $ newIORef $ EntityPointer (# 0#, 0# #)

  liftIO $ insertEntityIntoTables (ProcessedBundleData {elements = []}) world.tables (ArchetypeId 0) (entity, entityPointer)

  liftIO $ insertPointer entity entityPointer world.entities

  ChangeResult {requiredComponentsAdded} <- changeArchetype entity archetype (Just bundleD)

  unless world.prefs.supressEvents $ do
    let d = ProcessedBundleData $ requiredComponentsAdded ++ bundleD.elements
    triggerAddEvent d entity
    triggerSetEvent d entity

-- Spawn an entity as having no components and then immediately insert a bundle on it. Useful for certain engine internals, should be avoided.
spawnEntityByInsert :: (Bundle b) => Entity -> b -> System ()
spawnEntityByInsert entity bundle = do
  world <- unsafeGetWorld

  entityPointer <- liftIO $ newIORef $ EntityPointer (# 0#, 0# #)
  liftIO $ insertEntityIntoTables (ProcessedBundleData {elements = []}) world.tables (ArchetypeId 0) (entity, entityPointer)
  liftIO $ insertPointer entity entityPointer world.entities
  insert bundle entity

  insertNew (Name (show entity)) entity

spawnObserverOrdered :: forall e. (Event e) => Observer e -> Int -> System ()
spawnObserverOrdered observer order = do
  void $ spawn (observer, ObserverOrder order)

spawnObserver :: forall e. (Event e) => Observer e -> System ()
spawnObserver e = spawnObserverOrdered e 0

-- | Spawn an entity given a bundle of components, in IO.
spawnIO :: (Bundle b) => World -> b -> IO Entity
spawnIO world bundle =
  do
    entity <- liftIO $ getNewEntity world.entities

    runSystem (spawnEntity entity bundle) world
    return entity

-- | Despawn an entity. This will trigger the @OnRemove@ events and hooks on all its components.
despawn :: Entity -> System ()
despawn entity =
  do
    world <- unsafeGetWorld
    pointer <- liftIO $ getPointer entity world.entities
    case pointer of
      Nothing -> warn $ "Despawn failed: Entity " <> T.show entity <> " is not alive."
      Just pointer -> do
        let Tables tables = world.tables

        (EntityPointer (# archetypeId, _ #)) <- liftIO $ readIORef pointer

        table <- Vec.read tables (I# archetypeId)

        c <- liftIO $ collectComponentIdsFromTable table
        triggerRemoveEvent c entity

        (EntityPointer (# newArchetypeId, newRowIndex #)) <- liftIO $ readIORef pointer

        table <- Vec.read tables (I# newArchetypeId)
        void $ liftIO $ takeComponentsFromTable (EntityPointer (# newArchetypeId, newRowIndex #)) table
        liftIO $ removeEntity entity world.entities

module Mischief.ECS.World.Spawn where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT))
import Data.Data
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Stack
import Mischief.ECS.Archetypes.Graph (getArchetypeOnSpawn)
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Common
import Mischief.ECS.Entities
import Mischief.ECS.EventDef
import Mischief.ECS.Hidden
import Mischief.ECS.Log
import Mischief.ECS.Observer
import Mischief.ECS.Tables
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

spawnDefer :: (Bundle b) => b -> ParSystem Entity
spawnDefer bundle = do
  world <- unsafeGetWorld
  entity <- liftIO $ getNewEntity world.entities

  defer $ spawnEntity entity bundle
  return entity

data SpawnEventsSettings = WithSpawnEvents | WithoutSpawnEvents

spawnEntity :: (HasCallStack, Bundle b) => Entity -> b -> System ()
spawnEntity entity bundle = do
  world <- unsafeGetWorld
  let BundleData {elements} = addComponentToBundleData (Name (show entity)) $ bundleData bundle

  currentTick <- liftIO $ readIORef world.tick
  bundleD <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

  archetype <- getArchetypeOnSpawn $ map (\x -> x.id) bundleD.elements

  entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

  liftIO $ insertEntityIntoTables (ProcessedBundleData {elements = []}) world.tables (ArchetypeId 0) (entity, entityPointer)

  liftIO $ insertPointer entity entityPointer world.entities

  ChangeResult {requiredComponentsAdded} <- changeArchetype entity archetype (Just bundleD)

  unless world.prefs.supressEvents $ do
    triggerInsertEvent (ProcessedBundleData $ requiredComponentsAdded ++ bundleD.elements) entity

-- insertNew (Name (show entity)) entity

spawnEntityByInsert :: (Bundle b) => Entity -> b -> System ()
spawnEntityByInsert entity bundle = do
  world <- unsafeGetWorld

  entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

  liftIO $ insertEntityIntoTables (ProcessedBundleData {elements = []}) world.tables (ArchetypeId 0) (entity, entityPointer)

  liftIO $ insertPointer entity entityPointer world.entities

  insert bundle entity

  insertNew (Name (show entity)) entity

spawnObserverOrdered :: forall e. (Event e) => Observer e -> Int -> System ()
spawnObserverOrdered observer order = do
  void $ spawn (observer, ObserverOrder order)

spawnObserver :: forall e. (Event e) => Observer e -> System ()
spawnObserver e = spawnObserverOrdered e 0

-- | Spawn an entity given a bundle of components.
spawnIO :: (Bundle b) => World -> b -> IO Entity
spawnIO world bundle =
  do
    entity <- liftIO $ getNewEntity world.entities

    runSystem (spawnEntity entity bundle) world
    return entity

-- | Despawn an entity.
despawn :: Entity -> System ()
despawn entity =
  do
    world <- unsafeGetWorld
    pointer <- liftIO $ getPointer entity world.entities
    case pointer of
      Nothing -> warn $ "Despawn failed: Entity " <> text entity <> " is not alive."
      Just pointer -> do
        let Tables tables = world.tables

        tables' <- liftIO $ readIORef tables
        currentPointer <- liftIO $ readIORef pointer

        case Map.lookup currentPointer.archetypeId tables' of
          Nothing -> undefined
          Just table -> do
            c <- liftIO $ collectComponentIdsFromTable table
            triggerRemoveEvent c entity

        tables' <- liftIO $ readIORef tables
        newPointer <- liftIO $ readIORef pointer

        case Map.lookup newPointer.archetypeId tables' of
          Nothing -> undefined
          Just table -> do
            void $ liftIO $ takeComponentsFromTable newPointer table
            liftIO $ removeEntity entity world.entities

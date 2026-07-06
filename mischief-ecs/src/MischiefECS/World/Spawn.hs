module MischiefECS.World.Spawn where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT))
import Data.Data
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import {-# SOURCE #-} MischiefECS.Archetypes.Graph (getArchetypeOnSpawn)
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Common
import {-# SOURCE #-} MischiefECS.Components.Spawn (getOrAddComponentId)
import MischiefECS.Entities
import MischiefECS.Events
import MischiefECS.Hidden
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Change
import MischiefECS.World.Defer
import MischiefECS.World.Insert
import MischiefECS.World.Utils

-- | Spawn an entity given a bundle of components.
spawn :: (Bundle b) => b -> System Entity
spawn bundle =
  do
    world <- ask
    entity <- liftIO $ getNewEntity world.entities

    spawnEntity entity bundle
    return entity

spawnDefer :: (Bundle b) => b -> ParSystem Entity
spawnDefer bundle = do
  ParWorld {world} <- ask
  entity <- liftIO $ getNewEntity world.get.entities

  defer $ spawnEntity entity bundle
  return entity

data SpawnEventsSettings = WithSpawnEvents | WithoutSpawnEvents

spawnEntity :: (Bundle b) => Entity -> b -> System ()
spawnEntity entity bundle = do
  world <- ask
  let BundleData {elements, required} = addComponentToBundleData (Name (show entity)) $ bundleData bundle

  currentTick <- liftIO $ readIORef world.tick
  bundleD <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

  archetype <- getArchetypeOnSpawn $ map (\x -> x.id) bundleD.elements

  entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

  liftIO $ insertEntityIntoTables (ProcessedBundleData {elements = []}) world.tables (ArchetypeId 0) (entity, entityPointer)

  liftIO $ insertPointer entity entityPointer world.entities

  -- insert bundle entity
  changeArchetype entity archetype (Just bundleD)

-- insertNew (Name (show entity)) entity

spawnEntityByInsert :: (Bundle b) => Entity -> b -> System ()
spawnEntityByInsert entity bundle = do
  world <- ask

  entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

  liftIO $ insertEntityIntoTables (ProcessedBundleData {elements = []}) world.tables (ArchetypeId 0) (entity, entityPointer)

  liftIO $ insertPointer entity entityPointer world.entities

  insert bundle entity

  insertNew (Name (show entity)) entity

spawnObserver :: forall e. (Event e) => Observer e -> Maybe ObserverOrder -> System ()
spawnObserver observer order' = do
  let order = fromMaybe (ObserverOrder 0) order'

  _ <- spawn ((observer, order), EventProxy @e)

  return ()

-- | Spawn an entity given a bundle of components.
spawnIO :: (Bundle b) => World -> b -> IO Entity
spawnIO world bundle =
  do
    entity <- liftIO $ getNewEntity world.entities

    runSystem (spawnEntity entity bundle) world
    return entity
module MischiefECS.World.Spawn where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.IORef
import Data.Map qualified as Map
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Events
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Insert

-- | Spawn an entity given a bundle of components.
spawn :: (Bundle b) => b -> System Entity
spawn bundle =
  do
    world <- ask

    entityIndex <- liftIO $ readIORef world.entities.counter
    let entity = Entity entityIndex
    liftIO $ modifyIORef world.entities.counter (+ 1)

    spawnEntity entity bundle
    return entity

spawnEntity :: (Bundle b) => Entity -> b -> System ()
spawnEntity entity bundle = do
  world <- ask
  let BundleData {elements, required} = addComponentToBundleData entity $ bundleData bundle

  currentTick <- liftIO $ readIORef world.tick
  bundle <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} $ Set.union elements required

  triggerInsertEvent bundle entity

  archetypeId <- liftIO $ archetypeOfProcessedBundle world.archetypes bundle

  entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

  liftIO $ insertEntityIntoTables bundle world.tables archetypeId (entity, entityPointer)

  liftIO $ modifyIORef' world.entities.pointers $ Map.insert entity entityPointer

  insertNew (Name (show entity)) entity

spawnObserver :: forall e. (Event e) => Observer e -> System ()
spawnObserver observer = do
  _ <- spawn (observer, EventProxy @e)
  return ()

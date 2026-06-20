module MischiefECS.World.Change where

import Control.Monad.Reader (MonadIO (liftIO), ask)
import Data.Foldable
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query
import MischiefECS.World.Utils

changeArchetype :: Entity -> ArchetypeData -> Maybe ProcessedBundleData -> System ()
changeArchetype entity newArchetype insertedBundle = do
  world <- ask
  currentTick <- liftIO $ readIORef world.tick

  Just pointer <- liftIO $ getPointer entity world.entities
  pointer' <- liftIO $ readIORef pointer

  tables <- liftIO $ readIORef world.tables.inner
  let table = fromMaybe undefined $ Map.lookup pointer'.archetypeId tables

  collected <- liftIO $ takeComponentsFromTable pointer' table

  newElements'' <-
    mapM
      ( \component -> do
          case insertedBundle of
            Nothing -> maybe undefined return (find (\x -> x.id == component) collected.elements)
            Just bundle ->
              case find (\x -> x.id == component) bundle.elements of
                Just x -> return x
                Nothing ->
                  case find (\x -> x.id == component) collected.elements of
                    Just x -> return x
                    Nothing -> getDefault component
      )
      $ Set.toList newArchetype.components

  let newElements = case insertedBundle of
        Nothing -> ProcessedBundleData newElements''
        Just bundle ->
          let newElements' = setChangedTickOfComponents (ProcessedBundleData newElements'') (\id -> isInProcessedBundle bundle id && isInProcessedBundle collected id) currentTick
           in setAddedTickOfComponents newElements' (\id -> isInProcessedBundle bundle id && not (isInProcessedBundle collected id)) currentTick

  liftIO $ insertEntityIntoTables newElements world.tables newArchetype.id (entity, pointer)

getDefault :: ComponentId -> System ProcessedBundleElement
getDefault component = do
  world <- ask

  Just x <- get @DefaultValue component.id
  let (DefaultValue value) = x.value

  return
    ProcessedBundleElement
      { id = component,
        component =
          ComponentData
            { value,
              ticks = ComponentTicks {changed = world.currentSystemTick, added = world.currentSystemTick}
            }
      }

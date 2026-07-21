module Mischief.ECS.World.Change where

import Control.Monad.Reader (MonadIO (liftIO), ask)
import Data.Foldable
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Mischief.ECS.App.SystemDef
import Mischief.ECS.Archetypes
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Utils

data ChangeResult = ChangeResult
  { requiredComponentsAdded :: [ProcessedBundleElement],
    newComponents :: [ProcessedBundleElement]
  }

changeArchetype :: (HasCallStack) => Entity -> ArchetypeData -> Maybe ProcessedBundleData -> System ChangeResult
changeArchetype entity newArchetype insertedBundle = do
  world <- unsafeGetWorld
  currentTick <- liftIO $ readIORef world.tick

  reqAdded <- liftIO $ newIORef []

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
                    Nothing -> do
                      r <- getDefault component
                      liftIO $ modifyIORef' reqAdded (++ [r])
                      return r
      )
      $ Set.toList newArchetype.components

  let newElements = case insertedBundle of
        Nothing -> ProcessedBundleData newElements''
        Just bundle ->
          let newElements' = setChangedTickOfComponents (ProcessedBundleData newElements'') (\id -> isInProcessedBundle bundle id && isInProcessedBundle collected id) currentTick
           in setAddedTickOfComponents newElements' (\id -> isInProcessedBundle bundle id && not (isInProcessedBundle collected id)) currentTick

  liftIO $ insertEntityIntoTables newElements world.tables newArchetype.id (entity, pointer)

  requiredComponentsAdded <- liftIO $ readIORef reqAdded
  return ChangeResult {requiredComponentsAdded}

getDefault :: ComponentId -> System ProcessedBundleElement
getDefault component = do
  world <- unsafeGetWorld

  Just x <- get (C @DefaultValue) component.id
  let dv = value x
  let (DefaultValue value) = dv

  let (SystemId sys) = world.systemId
  currentSystemTick <- fromMaybe (SystemTick $ Tick 0) <$> get (Val $ C @SystemTick) sys

  return
    ProcessedBundleElement
      { id = component,
        component =
          ComponentData
            { value,
              ticks = ComponentTicks {changed = currentSystemTick.inner, added = currentSystemTick.inner}
            }
      }

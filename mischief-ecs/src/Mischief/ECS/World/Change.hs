module Mischief.ECS.World.Change where

import Control.Monad.Reader (MonadIO (liftIO), ask)
import Data.Foldable
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Base (Int (..), when)
import GHC.Stack (HasCallStack)
import Mischief.ECS.App.SystemDef
import Mischief.ECS.Archetypes
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
import Mischief.ECS.EntityDef
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.Vec qualified as Vec
import Mischief.ECS.World
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
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
  newAdded <- liftIO $ newIORef []

  Just pointer <- liftIO $ getPointer entity world.entities
  (EntityPointer (# archetypeId', rowIndex' #)) <- liftIO $ readIORef pointer

  table <- Vec.read world.tables.inner (I# archetypeId')

  collected <- liftIO $ takeComponentsFromTable (EntityPointer (# archetypeId', rowIndex' #)) table

  -- TODO OPT: this whole thing can be greatly optimized
  newElements'' <-
    mapM
      ( \component -> do
          case insertedBundle of
            Nothing -> maybe undefined return (find (\x -> x.id == component) collected.elements)
            Just bundle ->
              case find (\x -> x.id == component) bundle.elements of
                Just x -> do
                  case find (\x -> x.id == component) collected.elements of
                    Nothing -> pure ()
                    Just y -> liftIO $ modifyIORef' newAdded (++ [y])
                  return x
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
  newComponents <- liftIO $ readIORef newAdded
  return ChangeResult {requiredComponentsAdded, newComponents}

getDefault :: ComponentId -> System ProcessedBundleElement
getDefault (ComponentId (# id, e #)) = do
  world <- unsafeGetWorld

  Just x <- get (C @DefaultValue) (Entity (# id, 0## #))
  let dv = value x
  let (DefaultValue value) = dv

  let (SystemId sys) = world.systemId
  currentSystemTick <- fromMaybe (SystemTick $ Tick (0, 0)) <$> get (Val $ C @SystemTick) sys

  return
    ProcessedBundleElement
      { id = ComponentId (# id, e #),
        component =
          ComponentData
            { value,
              ticks = ComponentTicks {changed = currentSystemTick.inner, added = currentSystemTick.inner}
            }
      }

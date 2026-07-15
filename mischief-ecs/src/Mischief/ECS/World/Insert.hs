{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.ECS.World.Insert where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.Data
import Data.Foldable (for_)
import Data.IORef
import Data.List hiding (insert)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Stack
import Mischief.ECS.Archetypes
import {-# SOURCE #-} Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import {-# SOURCE #-} Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Log
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Change
import Mischief.ECS.World.Prefs
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Utils

data Exception' = Exception' deriving (Show)

instance Exception Exception'

-- | Insert a bundle of components on an Entity.
--
-- If the entity already contains these components, their values will be
-- updated in-place instead of causing an archetype change.
insert :: (HasCallStack) => forall b. (Bundle b) => b -> Entity -> System ()
insert bundle entity =
  do
    world <- unsafeGetWorld
    pointer <- liftIO $ getPointer entity world.entities

    case pointer of
      Nothing -> warn $ "Insertion failed: Entity " <> text entity <> " is not alive."
      Just currentPointer -> do
        let BundleData {elements} = bundleData bundle

        currentTick <- liftIO $ readIORef world.tick

        bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements
        let newComponents = sort $ map (\x -> x.id) bundleData.elements

        currentPointerInternal <- liftIO $ readIORef currentPointer

        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables

        case Map.lookup currentPointerInternal.archetypeId tables of
          Nothing -> undefined
          Just currentTable -> do
            -- Simple case, no archetype change.
            if newComponents `isSubsequenceOf` currentTable.components
              then
                liftIO $ replaceComponentsIntoTable bundleData (Just currentTick) currentPointerInternal currentTable
              -- Complex case, archetype change.
              else do
                newArchetype <- getArchetypeOnInsert currentPointerInternal.archetypeId newComponents
                ChangeResult {requiredComponentsAdded} <- changeArchetype entity newArchetype (Just bundleData)

                unless world.prefs.supressEvents $
                  triggerInsertEvent (ProcessedBundleData requiredComponentsAdded) entity

        unless world.prefs.supressEvents $
          triggerInsertEvent bundleData entity

getOrInsert :: forall qd. (Queryable qd (Result qd), Bundle qd) => qd -> Entity -> System (Result qd)
getOrInsert val entity = do
  g <- get @qd entity
  case g of
    Just g -> return g
    Nothing -> do
      insert val entity
      return $ Result (val, entity)

resOrInsert :: forall c. (Queryable c (Result c), Component c, Bundle c) => c -> System (Result c)
resOrInsert val = do
  r <- res @c
  case r of
    Just r -> return r
    Nothing -> do
      insertRes val
      entity <- meta @c
      return $ Result (val, entity)

-- | Insert a bundle of components on an Entity.
--
-- Only the components that the entity doesn't already have will be inserted, and the rest ignored.
insertNew :: forall b. (Bundle b) => b -> Entity -> System ()
insertNew bundle entity =
  do
    world <- unsafeGetWorld
    pointer <- liftIO $ getPointer entity world.entities

    case pointer of
      Nothing -> warn $ "Insertion failed: Entity " <> text entity <> " is not alive."
      Just currentPointer -> do
        let BundleData {elements} = bundleData bundle

        currentTick <- liftIO $ readIORef world.tick

        bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

        currentPointerInternal <- liftIO $ readIORef currentPointer

        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables

        case Map.lookup currentPointerInternal.archetypeId tables of
          Nothing -> undefined
          Just currentTable -> do
            let newComponents = ProcessedBundleData $ filter (\c -> c.id `notElem` currentTable.components) bundleData.elements

            unless (null newComponents.elements) $ do
              newArchetype <- getArchetypeOnInsert currentPointerInternal.archetypeId $ map (\x -> x.id) newComponents.elements
              ChangeResult {requiredComponentsAdded} <- changeArchetype entity newArchetype (Just bundleData)

              unless world.prefs.supressEvents $
                triggerInsertEvent (ProcessedBundleData requiredComponentsAdded) entity

            unless world.prefs.supressEvents $
              triggerInsertEvent newComponents entity

-- | Insert a resource into this world. If the resource already exists, its value will be overwritten.
insertRes :: forall r. (Component r, Bundle r) => r -> System ()
insertRes res = do
  entity <- meta @r
  insert res entity

-- | Set the value of a component obtained as query result.
--
-- Note that the local 'Result' won't be mutated.
-- You'll need to query the component again or use 'update' to update the current result.
set :: (Bundle c) => Result c -> c -> System ()
set !result !newValue = Mischief.ECS.World.Insert.insert newValue (entityOf result)

setIfNeq :: forall c. (Bundle c, Queryable c (Result c), Eq c) => Result c -> c -> System ()
setIfNeq !result !newValue = do
  curr <- get @c (entityOf result)
  case curr of
    Nothing -> warn $ "SetIfNeq failed: Entity " <> text (entityOf result) <> " is not alive."
    Just curr ->
      when (value curr /= newValue) $
        set result newValue

-- | Update the value of a 'Result'.
--
-- Useful if you've done changed to the component and want to grab the live value
-- without re-querying.
update :: forall c output. (Queryable c output) => Result c -> System (Maybe output)
update c = get @c (entityOf c)

triggerInsertEvent :: ProcessedBundleData -> Entity -> System ()
triggerInsertEvent bundle entity =
  for_ bundle.elements $ \x -> do
    case x.id.entity of
      Nothing ->
        triggerInsertEventC x.component.value entity
      Just target ->
        triggerInsertEventR x.component.value target entity

triggerInsertEventC :: ErasedComponent -> Entity -> System ()
triggerInsertEventC (ErasedComponent (_ :: c)) entity =
  runEvent $ eraseEvent $ OnInsert @c entity

triggerInsertEventR :: ErasedComponent -> Entity -> Entity -> System ()
triggerInsertEventR (ErasedComponent (_ :: c)) target entity = do
  runEvent $ eraseEvent $ OnInsertRel @c entity target
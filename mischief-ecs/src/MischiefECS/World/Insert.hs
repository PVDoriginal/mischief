module MischiefECS.World.Insert where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.List
import Data.Map qualified as Map
import Data.Set qualified as Set
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World

-- | Insert a bundle of components on an Entity.
--
-- If the entity already contains these components, their values will be
-- updated in-place instead of causing an archetype change.
insert :: (Bundle b) => b -> Entity -> System ()
insert bundle entity =
  do
    world <- ask
    let BundleData {elements, required} = bundleData bundle

    currentTick <- liftIO $ readIORef world.tick

    bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements
    let newComponents = sort $ map (\x -> x.id) bundleData.elements

    entityPointers <- liftIO $ readIORef world.entities.pointers
    case Map.lookup entity entityPointers of
      Nothing -> undefined
      Just currentPointer -> do
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
                collectedComponents <- liftIO $ takeComponentsFromTable currentPointerInternal currentTable

                empty <- liftIO $ tableIsEmpty currentTable
                when empty $ do
                  liftIO $ removeTableAndArchetype world currentPointerInternal.archetypeId

                let newBundle = combineProcessedBundles collectedComponents bundleData
                archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

                let newBundle' = setChangedTickOfComponents newBundle (isInProcessedBundle collectedComponents) currentTick
                let newBundle'' = setAddedTickOfComponents newBundle' (\id -> isInProcessedBundle bundleData id && not (isInProcessedBundle collectedComponents id)) currentTick

                liftIO $ insertEntityIntoTables newBundle'' world.tables archetype (entity, currentPointer)
                newPointer <- liftIO $ readIORef currentPointer

                let Tables tables = world.tables
                tables <- liftIO $ readIORef tables

                case Map.lookup newPointer.archetypeId tables of
                  Nothing -> undefined
                  Just newTable -> do
                    liftIO $ replaceComponentsIntoTable bundleData Nothing newPointer newTable

    unless (null required) $ insertNew (BundleData {elements = required, required = Set.empty}) entity

-- | Insert a bundle of components on an Entity.
--
-- Only the components that the entity doesn't already have will be inserted, and the rest ignored.
insertNew :: (Bundle b) => b -> Entity -> System ()
insertNew bundle entity =
  do
    world <- ask
    let BundleData {elements, required} = bundleData bundle

    currentTick <- liftIO $ readIORef world.tick

    bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} (Set.union elements required)
    let components = sort $ map (\x -> x.id) bundleData.elements

    entityPointers <- liftIO $ readIORef world.entities.pointers
    case Map.lookup entity entityPointers of
      Nothing -> undefined
      Just currentPointer -> do
        currentPointerInternal <- liftIO $ readIORef currentPointer

        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables

        case Map.lookup currentPointerInternal.archetypeId tables of
          Nothing -> undefined
          Just currentTable -> do
            let newComponents = filter (\c -> c `notElem` currentTable.components) components

            unless (null newComponents) $ do
              collectedComponents <- liftIO $ takeComponentsFromTable currentPointerInternal currentTable

              empty <- liftIO $ tableIsEmpty currentTable
              when empty $ do
                liftIO $ removeTableAndArchetype world currentPointerInternal.archetypeId

              let newBundle = combineProcessedBundles collectedComponents bundleData
              archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

              liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, currentPointer)

-- | Set the value of a component obtained as query result.
--
-- Note that the local 'ComponentResult' won't be mutated.
-- You'll need to query the component again or use 'get' to update the current result.
set :: (Bundle c) => ComponentResult c -> c -> System ()
set !result !newValue = MischiefECS.World.Insert.insert newValue result.entity

-- | Update the value of a 'ComponentResult'.
--
-- Useful if you've done changed to the component and want to grab the live value
-- without re-querying.
get :: ComponentResult c -> System (ComponentResult c)
get = undefined

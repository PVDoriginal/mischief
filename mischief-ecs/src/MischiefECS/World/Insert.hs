{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module MischiefECS.World.Insert where

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
import MischiefECS.Archetypes
import {-# SOURCE #-} MischiefECS.Archetypes.Graph
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Internal
import {-# SOURCE #-} MischiefECS.Components.Spawn
import MischiefECS.Entities
import MischiefECS.Entities.Internal
import MischiefECS.Events
import MischiefECS.Tables
import MischiefECS.Vec qualified as Vec
import MischiefECS.World
import MischiefECS.World.Change
import {-# SOURCE #-} MischiefECS.World.Spawn
import MischiefECS.World.Utils

-- | Insert a bundle of components on an Entity.
--
-- If the entity already contains these components, their values will be
-- updated in-place instead of causing an archetype change.
insert :: forall b. (Bundle b) => b -> Entity -> System ()
insert bundle entity =
  do
    world <- ask
    let BundleData {elements, required} = bundleData bundle

    currentTick <- liftIO $ readIORef world.tick

    bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements
    let newComponents = sort $ map (\x -> x.id) bundleData.elements

    pointer <- liftIO $ getPointer entity world.entities
    case pointer of
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
                newArchetype <- getArchetypeOnInsert currentPointerInternal.archetypeId newComponents
                changeArchetype entity newArchetype (Just bundleData)

    triggerInsertEvent bundleData entity
    unless (null required) $ insertNew (BundleData {elements = required, required = Set.empty}) entity

-- | Insert a bundle of components on an Entity.
--
-- Only the components that the entity doesn't already have will be inserted, and the rest ignored.
insertNew :: forall b. (Bundle b) => b -> Entity -> System ()
insertNew bundle entity =
  do
    world <- ask
    let BundleData {elements, required} = bundleData bundle

    currentTick <- liftIO $ readIORef world.tick

    bundleData <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} (Set.union elements required)

    pointer <- liftIO $ getPointer entity world.entities
    case pointer of
      Nothing -> undefined
      Just currentPointer -> do
        currentPointerInternal <- liftIO $ readIORef currentPointer

        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables

        case Map.lookup currentPointerInternal.archetypeId tables of
          Nothing -> undefined
          Just currentTable -> do
            let newComponents = ProcessedBundleData $ filter (\c -> c.id `notElem` currentTable.components) bundleData.elements

            unless (null newComponents.elements) $ do
              newArchetype <- getArchetypeOnInsert currentPointerInternal.archetypeId $ map (\x -> x.id) newComponents.elements
              changeArchetype entity newArchetype (Just bundleData)

              triggerInsertEvent newComponents entity

findResourceArchetype :: forall r. (Component r) => r -> System (Maybe ArchetypeId)
findResourceArchetype r =
  do
    world <- ask
    componentId <- getOrAddComponentId (ComponentType $ Proxy @r) world.components
    -- archetypes <- liftIO $ findMatchingArchetypes [componentId] world.archetypes world.components
    undefined

-- return $ case archetypes of
--   [(_, x)] -> Just x
--   [] -> Nothing
--   _ -> undefined

-- | Insert a resource into this world. If the resource already exists, its value will be overwritten.
insertRes :: forall r. (Component r) => r -> System ()
insertRes res = do
  entity <- entityOf @r
  insert res entity

i :: (Component r) => r -> System ()
i r = do
  let l = undefined
  insert r l

--   do
--     world <- ask
--     currentTick <- liftIO $ readIORef world.tick

--     resourceEntity <- liftIO $ newIORef Nothing

--     archetype <- findResourceArchetype r
--     case archetype of
--       Just archetype -> do
--         let Tables tables = world.tables
--         tables <- liftIO $ readIORef tables

--         case Map.lookup archetype tables of
--           Nothing -> undefined
--           Just table -> do
--             let bundleData = bundleDataRes r
--             bundle <- liftIO $ processBundleElements world ComponentTicks {added = currentTick, changed = currentTick} bundleData.elements
--             liftIO $ replaceComponentsIntoTable bundle (Just currentTick) EntityPointer {archetypeId = archetype, rowIndex = 0} table

--             (entity, _) <- Vec.read table.entities 0
--             triggerInsertEvent bundle entity

--             liftIO $ writeIORef resourceEntity $ Just entity
--       Nothing -> do
--         entity <- liftIO $ getNewEntity world.entities
--         liftIO $ writeIORef resourceEntity $ Just entity

--         let BundleData {elements} = addComponentToBundleData (Name "Resource") $ addComponentToBundleData entity $ bundleDataRes r

--         bundle <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

--         archetypeId <- liftIO $ archetypeOfProcessedBundle world.archetypes world.components bundle
--         entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

--         liftIO $ insertResourceIntoTables bundle currentTick world.tables archetypeId (entity, entityPointer)

--         liftIO $ insertPointer entity entityPointer world.entities
--         triggerInsertEvent bundle entity

--         insertNew (Name $ show entity) entity

--     entity <- liftIO $ readIORef resourceEntity
--     case entity of
--       Nothing -> undefined
--       Just entity -> do
--         -- runEvent $ eraseEvent $ OnInsert @r entity

--         let BundleData {required} = bundleDataRes r
--         unless (null required) $ insertNew (BundleData {elements = required, required = Set.empty}) entity

-- applySystem (Proxy @b) $ triggerInsertEvent entity

-- | Set the value of a component obtained as query result.
--
-- Note that the local 'ComponentResult' won't be mutated.
-- You'll need to query the component again or use 'update' to update the current result.
set :: (Bundle c) => ComponentResult c -> c -> System ()
set !result !newValue = MischiefECS.World.Insert.insert newValue result.entity

-- | Update the value of a 'ComponentResult'.
--
-- Useful if you've done changed to the component and want to grab the live value
-- without re-querying.
update :: ComponentResult c -> System (ComponentResult c)
update = undefined

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
  runEvent $ eraseEvent $ OnInsertR @c entity target
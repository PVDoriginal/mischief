{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.World.Utils where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Internal
import {-# SOURCE #-} MischiefECS.Components.Spawn
import MischiefECS.Entities
import MischiefECS.Events.Internal
import MischiefECS.Tables
import MischiefECS.World
import {-# SOURCE #-} MischiefECS.World.Spawn (spawnIO)

-- | Process a 'BundleElement', turning its 'TypeRep' into a 'ComponentId'.
processBundleElement :: World -> ComponentTicks -> BundleElement -> IO ProcessedBundleElement
processBundleElement world ticks BundleElement {rep = (ComponentRep r), component} =
  do
    id <- runSystem (getOrAddComponentId r world.components) world
    return
      ProcessedBundleElement
        { id,
          component =
            ComponentData
              { value = component,
                ticks
              }
        }
processBundleElement world ticks BundleElement {rep = (PairRep (r, entity)), component} =
  do
    id <- runSystem (getOrAddPairId (Pair (r, entity)) world.components) world
    return
      ProcessedBundleElement
        { id,
          component =
            ComponentData
              { value = component,
                ticks
              }
        }

-- | Process a set of 'BundleElement's into a 'ProcessedBundleData'.
processBundleElements :: World -> ComponentTicks -> Set BundleElement -> IO ProcessedBundleData
processBundleElements world ticks elements =
  do
    elements <- mapM (processBundleElement world ticks) (Set.toList elements)
    return
      ProcessedBundleData {elements}

-- | Combine two 'ProcessedBundleData's, merging their sets of elements.
combineProcessedBundles :: ProcessedBundleData -> ProcessedBundleData -> ProcessedBundleData
combineProcessedBundles bundle1 bundle2 =
  let elements = Set.toList $ Set.union (Set.fromList bundle1.elements) (Set.fromList bundle2.elements)
   in ProcessedBundleData {elements}

-- | Check if a 'ComponentId' is inside a 'ProcessedBundleData'.
isInProcessedBundle :: ProcessedBundleData -> ComponentId -> Bool
isInProcessedBundle ProcessedBundleData {elements} id = id `elem` map (\element -> element.id) elements

-- | Sets the change tick of certain elements of the bundle to the specified 'Tick'.
setChangedTickOfComponents :: ProcessedBundleData -> (ComponentId -> Bool) -> Tick -> ProcessedBundleData
setChangedTickOfComponents ProcessedBundleData {elements} shouldChange tick =
  ProcessedBundleData
    { elements =
        map
          ( \ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}} ->
              if shouldChange id
                then ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed = tick}}}
                else ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}}
          )
          elements
    }

-- | Sets the added tick of certain elements of the bundle to the specified 'Tick'.
setAddedTickOfComponents :: ProcessedBundleData -> (ComponentId -> Bool) -> Tick -> ProcessedBundleData
setAddedTickOfComponents ProcessedBundleData {elements} shouldChange tick =
  ProcessedBundleData
    { elements =
        map
          ( \ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}} ->
              if shouldChange id
                then ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added = tick, changed}}}
                else ProcessedBundleElement {id, component = ComponentData {value, ticks = ComponentTicks {added, changed}}}
          )
          elements
    }

removeComponentFromProcessedBundle :: ComponentId -> ProcessedBundleData -> ProcessedBundleData
removeComponentFromProcessedBundle componentId bundle =
  do
    let elements = filter (\x -> x.id /= componentId) bundle.elements
     in ProcessedBundleData {elements}

-- | Despawn an entity.
despawn :: Entity -> System ()
despawn entity =
  do
    world <- ask
    pointer <- liftIO $ getPointer entity world.entities
    case pointer of
      Nothing -> undefined
      Just pointer -> do
        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables
        currentPointer <- liftIO $ readIORef pointer

        case Map.lookup currentPointer.archetypeId tables of
          Nothing -> undefined
          Just table -> do
            liftIO $ removeComponentsFromTable currentPointer table

            empty <- liftIO $ tableIsEmpty table

            when empty $ do
              liftIO $ removeTableAndArchetype world currentPointer.archetypeId

            liftIO $ removeEntity entity world.entities

removeTableAndArchetype :: World -> ArchetypeId -> IO ()
removeTableAndArchetype !world !archetype =
  do
    removeArchetypeId archetype world.archetypes world.components
    removeTable archetype world.tables

-- | Remove a component from an entity.
removeComponentFromEntity :: forall c. (Component c) => Entity -> System ()
removeComponentFromEntity entity =
  do
    world <- ask
    componentId <- getOrAddComponentId (ComponentType $ Proxy @c) world.components
    removeFromEntity componentId entity

removeRelationshipFromEntity :: forall c. (Component c) => Entity -> Entity -> System ()
removeRelationshipFromEntity target entity = do
  world <- ask
  componentId <- getOrAddPairId (Pair (ComponentType $ Proxy @c, target)) world.components
  removeFromEntity componentId entity

removeFromEntity :: ComponentId -> Entity -> System ()
removeFromEntity componentId entity = do
  world <- ask
  pointer <- liftIO $ getPointer entity world.entities

  case pointer of
    Nothing -> undefined
    Just pointer -> do
      let Tables tables = world.tables
      tables <- liftIO $ readIORef tables
      currentPointer <- liftIO $ readIORef pointer

      case Map.lookup currentPointer.archetypeId tables of
        Nothing -> undefined
        Just currentTable -> do
          when (componentId `elem` currentTable.components) $ do
            collectedComponents <- liftIO $ takeComponentsFromTable currentPointer currentTable

            empty <- liftIO $ tableIsEmpty currentTable
            when empty $ do
              liftIO $ removeTableAndArchetype world currentPointer.archetypeId

            let newBundle = removeComponentFromProcessedBundle componentId collectedComponents
            archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes world.components newBundle

            liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, pointer)

tryGetEntityRelationshipCollection :: forall c. (Component c) => World -> Entity -> IO (Maybe (RelationshipCollection c))
tryGetEntityRelationshipCollection world entity =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return Nothing
      Just componentId -> do
        pointer <- getPointer entity world.entities
        case pointer of
          Nothing -> do
            return Nothing
          Just pointer ->
            do
              pointer <- readIORef pointer
              tryGetRelationshipCollectionFromTables world.tables entity pointer componentId

tryGetEntityComponent :: forall c. (Component c) => World -> Entity -> IO (Maybe c)
tryGetEntityComponent world entity =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components

    case componentId of
      Nothing -> return Nothing
      Just componentId -> do
        pointer <- getPointer entity world.entities

        case pointer of
          Nothing -> return Nothing
          Just pointer ->
            do
              pointer <- readIORef pointer
              tryGetComponentFromTables world.tables pointer componentId

tryGetRelationshipCollections :: forall c. (Component c) => World -> [ArchetypeId] -> IO [RelationshipCollection c]
tryGetRelationshipCollections world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return []
      Just componentId ->
        tryGetRelationshipCollectionsFromTables world.tables archetypes componentId

tryGetComponents :: forall c. (Component c) => World -> [ArchetypeId] -> IO [ComponentResult c]
tryGetComponents world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    case componentId of
      Nothing -> return []
      Just componentId ->
        tryGetComponentsFromTables world.tables archetypes componentId

tryGetTicks :: TypeRep -> World -> [ArchetypeId] -> IO [ComponentTicks]
tryGetTicks rep world archetypes =
  do
    componentId <- getComponentId rep world.components
    case componentId of
      Nothing -> return []
      Just componentId ->
        tryGetTicksFromTables world.tables archetypes componentId

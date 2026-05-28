{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.World where

import Control.Applicative (WrappedMonad (unwrapMonad))
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.Functor
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Entities
import MischiefECS.Tables

data World = World
  { archetypes :: Archetypes,
    components :: Components,
    entities :: Entities,
    tables :: Tables,
    deferred :: IORef [System ()],
    tick :: IORef Tick,
    lastSystemTick :: Tick,
    currentSystemTick :: Tick,
    systemId :: SystemId
  }

newtype SystemId = SystemId Int deriving (Show, Eq, Ord)

newWorld :: IO World
newWorld = do
  archetypes <- emptyArchetypes
  components <- emptyComponents
  entities <- emptyEntities
  tables <- emptyTables
  deferred <- newIORef []
  tick <- newIORef (Tick 0)

  return
    World
      { archetypes,
        components,
        entities,
        tables,
        deferred,
        tick,
        lastSystemTick = Tick 0,
        currentSystemTick = Tick 0,
        systemId = SystemId 0
      }

setSystemTicks :: Tick -> Tick -> World -> World
setSystemTicks lastSystemTick currentSystemTick World {archetypes, components, entities, tables, deferred, tick, systemId} =
  World {archetypes, components, entities, tables, deferred, tick, lastSystemTick, currentSystemTick, systemId}

tick :: System ()
tick = do
  world <- ask
  liftIO $ modifyIORef' world.tick (\(Tick x) -> Tick $ x + 1)

processBundleElement :: World -> ComponentTicks -> BundleElement -> IO ProcessedBundleElement
processBundleElement world ticks BundleElement {rep, component} =
  do
    id <- getComponentId rep world.components
    return
      ProcessedBundleElement
        { id,
          component =
            ComponentData
              { value = component,
                ticks
              }
        }

processBundleElements :: World -> ComponentTicks -> Set BundleElement -> IO ProcessedBundleData
processBundleElements world ticks elements =
  do
    elements <- mapM (processBundleElement world ticks) (Set.toList elements)
    return
      ProcessedBundleData {elements}

combineProcessedBundles :: World -> ProcessedBundleData -> ProcessedBundleData -> IO ProcessedBundleData
combineProcessedBundles world bundle1 bundle2 =
  let elements = Set.toList $ Set.union (Set.fromList bundle1.elements) (Set.fromList bundle2.elements)
   in do
        archetypeId <- getArchetypeId (map (\x -> x.id) elements) world.archetypes
        return
          ProcessedBundleData {elements}

isInProcessedBundle :: ProcessedBundleData -> ComponentId -> Bool
isInProcessedBundle ProcessedBundleData {elements} id = id `elem` map (\element -> element.id) elements

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

removeComponentFromProcessedBundle :: World -> ComponentId -> ProcessedBundleData -> IO ProcessedBundleData
removeComponentFromProcessedBundle world componentId bundle =
  do
    let elements = filter (\x -> x.id /= componentId) bundle.elements
    archetypeId <- getArchetypeId (map (\x -> x.id) elements) world.archetypes
    return ProcessedBundleData {elements}

findResourceArchetype :: (Component r, Storage r ~ ResourceStorage) => r -> System (Maybe ArchetypeId)
findResourceArchetype r =
  do
    world <- ask
    componentId <- liftIO $ getComponentId (typeOf r) world.components
    archetypes <- liftIO $ findMatchingArchetypes [componentId] world.archetypes

    return $ case archetypes of
      [(_, x)] -> Just x
      [] -> Nothing
      _ -> undefined

-- | Insert a resource into this world. If the resource already exists, its value will be overwritten.
insertResource :: (Component r, Storage r ~ ResourceStorage) => r -> System ()
insertResource r =
  do
    world <- ask
    currentTick <- liftIO $ readIORef world.tick

    archetype <- findResourceArchetype r
    case archetype of
      Just archetype -> do
        let Tables tables = world.tables
        tables <- liftIO $ readIORef tables

        case Map.lookup archetype tables of
          Nothing -> undefined
          Just table -> do
            let bundleData = bundleDataRes r
            bundle <- liftIO $ processBundleElements world ComponentTicks {added = currentTick, changed = currentTick} bundleData.elements
            liftIO $ replaceComponentsIntoTable bundle (Just currentTick) EntityPointer {archetypeId = archetype, rowIndex = 0} table
      Nothing -> do
        entityIndex <- liftIO $ readIORef world.entities.counter
        let entity = Entity entityIndex
        liftIO $ modifyIORef world.entities.counter (+ 1)

        let BundleData {elements} = addComponentToBundleData (Name "Resource") $ addComponentToBundleData entity $ bundleDataRes r

        bundle <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} elements

        archetypeId <- liftIO $ archetypeOfProcessedBundle world.archetypes bundle
        entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

        liftIO $ insertResourceIntoTables bundle currentTick world.tables archetypeId (entity, entityPointer)

        liftIO $ modifyIORef' world.entities.pointers $ Map.insert entity entityPointer

-- | Spawn an entity in this World given a bundle of components.
spawn :: (Bundle b) => b -> System Entity
spawn bundle =
  do
    world <- ask

    entityIndex <- liftIO $ readIORef world.entities.counter
    let entity = Entity entityIndex
    liftIO $ modifyIORef world.entities.counter (+ 1)

    let BundleData {elements, required} = addComponentToBundleData entity $ bundleData bundle

    currentTick <- liftIO $ readIORef world.tick
    bundle <- liftIO $ processBundleElements world ComponentTicks {changed = currentTick, added = currentTick} $ Set.union elements required

    archetypeId <- liftIO $ archetypeOfProcessedBundle world.archetypes bundle

    entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

    liftIO $ insertEntityIntoTables bundle world.tables archetypeId (entity, entityPointer)

    liftIO $ modifyIORef' world.entities.pointers $ Map.insert entity entityPointer

    insertNew (Name (show entity)) entity

    return entity

despawn :: Entity -> System ()
despawn entity =
  do
    world <- ask
    pointers <- liftIO $ readIORef world.entities.pointers
    case Map.lookup entity pointers of
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
    removeArchetypeId archetype world.archetypes
    removeTable archetype world.tables

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

                newBundle <- liftIO $ combineProcessedBundles world collectedComponents bundleData
                archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

                let newBundle' = setChangedTickOfComponents newBundle (isInProcessedBundle collectedComponents) currentTick
                let newBundle'' = setAddedTickOfComponents newBundle (\id -> isInProcessedBundle bundleData id && not (isInProcessedBundle collectedComponents id)) currentTick

                liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, currentPointer)
                newPointer <- liftIO $ readIORef currentPointer

                let Tables tables = world.tables
                tables <- liftIO $ readIORef tables

                case Map.lookup newPointer.archetypeId tables of
                  Nothing -> undefined
                  Just newTable -> do
                    liftIO $ replaceComponentsIntoTable bundleData Nothing newPointer newTable

    unless (null required) $ insertNew (BundleData {elements = required, required = Set.empty}) entity

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

              newBundle <- liftIO $ combineProcessedBundles world collectedComponents bundleData
              archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

              liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, currentPointer)

removeComponentFromEntity :: forall c. (Component c) => Entity -> System ()
removeComponentFromEntity entity =
  do
    world <- ask
    componentId <- liftIO $ getComponentId (typeRep $ Proxy @c) world.components
    entityPointers <- liftIO $ readIORef world.entities.pointers

    case Map.lookup entity entityPointers of
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

              newBundle <- liftIO $ removeComponentFromProcessedBundle world componentId collectedComponents
              archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

              liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, pointer)

tryGetEntityComponent :: forall c. (Component c) => World -> Entity -> IO (Maybe c)
tryGetEntityComponent world entity =
  do
    pointers <- readIORef world.entities.pointers
    components <- readIORef world.components.map

    let componentId = Map.lookup (typeRep $ Proxy @c) components

    case componentId of
      Nothing -> return Nothing
      Just componentId -> do
        let pointer = Map.lookup entity pointers

        case pointer of
          Nothing -> return Nothing
          Just pointer ->
            do
              pointer <- readIORef pointer
              tryGetComponentFromTables world.tables pointer componentId

tryGetComponents :: forall c. (Component c) => World -> [ArchetypeId] -> IO [ComponentResult c]
tryGetComponents world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @c) world.components
    tryGetComponentsFromTables world.tables archetypes componentId

tryGetTicks :: TypeRep -> World -> [ArchetypeId] -> IO [ComponentTicks]
tryGetTicks rep world archetypes =
  do
    componentId <- getComponentId rep world.components
    tryGetTicksFromTables world.tables archetypes componentId

set :: (Bundle c) => ComponentResult c -> c -> System ()
set !result !newValue = MischiefECS.World.insert newValue result.entity

class Modify c t where
  modify :: (Storage c ~ t) => ComponentResult c -> (c -> c) -> System ()

instance (Bundle c) => Modify c ComponentStorage where
  modify :: (Bundle c) => ComponentResult c -> (c -> c) -> System ()
  modify !result !f = MischiefECS.World.insert (f result.value) result.entity

instance (Component c, Storage c ~ ResourceStorage) => Modify c ResourceStorage where
  modify :: (Component c, Storage c ~ ResourceStorage) => ComponentResult c -> (c -> c) -> System ()
  modify !result !f = insertResource (f result.value)

type System = ReaderT World IO

defer :: System a -> System ()
defer !system = do
  world <- ask
  liftIO $ modifyIORef' world.deferred (++ [system $> ()])

flush :: System ()
flush = do
  world <- ask
  systems <- liftIO $ readIORef world.deferred

  sequence_ systems

  liftIO $ writeIORef world.deferred []

newtype Name = Name String deriving (Component)

instance Show Name where
  show :: Name -> String
  show (Name name) = show name

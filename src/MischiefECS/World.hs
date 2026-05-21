{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.World where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List
import Data.Map
import Data.Map qualified as Map
import Data.Proxy
import Data.Set
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Bundles
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Tables
import System.Info (arch)

data World = World
  { archetypes :: Archetypes,
    components :: Components,
    entities :: Entities,
    tables :: Tables,
    deferred :: IORef [System ()]
  }

newWorld :: IO World
newWorld = do
  archetypes <- emptyArchetypes
  components <- emptyComponents
  entities <- emptyEntities
  tables <- emptyTables
  deferred <- newIORef []

  return
    World
      { archetypes,
        components,
        entities,
        tables,
        deferred
      }

processBundleElement :: World -> BundleElement -> IO ProcessedBundleElement
processBundleElement world BundleElement {rep, component} =
  do
    id <- getComponentId rep world.components
    return
      ProcessedBundleElement
        { id,
          component
        }

processBundleData :: World -> BundleData -> IO ProcessedBundleData
processBundleData world (BundleData bundleData) =
  do
    elements <- mapM (processBundleElement world) (Set.toList bundleData)
    archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
    return
      ProcessedBundleData {elements}

combineProcessedBundles :: World -> ProcessedBundleData -> ProcessedBundleData -> IO ProcessedBundleData
combineProcessedBundles world bundle1 bundle2 =
  let elements = Set.toList $ Set.union (Set.fromList bundle1.elements) (Set.fromList bundle2.elements)
   in do
        archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
        return
          ProcessedBundleData {elements}

removeComponentFromProcessedBundle :: World -> ComponentId -> ProcessedBundleData -> IO ProcessedBundleData
removeComponentFromProcessedBundle world componentId bundle =
  do
    let elements = Prelude.filter (\x -> x.id /= componentId) bundle.elements
    archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
    return ProcessedBundleData {elements}

-- | Spawn an entity in this World given a bundle of components.
spawn :: (Bundle b) => b -> System Entity
spawn bundle =
  do
    world <- ask

    entityIndex <- liftIO $ readIORef world.entities.counter
    let entity = Entity entityIndex

    bundle <- liftIO $ processBundleData world $ addComponentToBundleData entity (bundleData bundle)

    archetypeId <- liftIO $ archetypeOfProcessedBundle world.archetypes bundle

    liftIO $ modifyIORef world.entities.counter (+ 1)

    entityPointer <- liftIO $ newIORef EntityPointer {archetypeId = ArchetypeId 0, rowIndex = 0}

    liftIO $ insertEntityIntoTables bundle world.tables archetypeId (entity, entityPointer)

    liftIO $ modifyIORef' world.entities.pointers $ Map.insert entity entityPointer

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
              liftIO $ removeArchetypeId currentPointer.archetypeId world.archetypes

            liftIO $ removeEntity entity world.entities

insert :: (Bundle b) => b -> Entity -> System ()
insert bundle entity =
  do
    world <- ask
    bundleData <- liftIO $ processBundleData world (bundleData bundle)
    let newComponents = sort $ Prelude.map (\x -> x.id) bundleData.elements

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
                liftIO $ replaceComponentsIntoTable bundleData currentPointerInternal currentTable
              -- Complex case, archetype change.
              else do
                collectedComponents <- liftIO $ takeComponentsFromTable currentPointerInternal currentTable
                newBundle <- liftIO $ combineProcessedBundles world collectedComponents bundleData
                archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

                liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, currentPointer)
                newPointer <- liftIO $ readIORef currentPointer

                let Tables tables = world.tables
                tables <- liftIO $ readIORef tables

                case Map.lookup newPointer.archetypeId tables of
                  Nothing -> undefined
                  Just newTable -> do
                    liftIO $ replaceComponentsIntoTable bundleData newPointer newTable

removeComponentFromEntity :: forall c -> (Component c) => Entity -> System ()
removeComponentFromEntity componentType entity =
  do
    world <- ask
    componentId <- liftIO $ getComponentId (typeRep $ Proxy @componentType) world.components
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
              newBundle <- liftIO $ removeComponentFromProcessedBundle world componentId collectedComponents
              archetype <- liftIO $ archetypeOfProcessedBundle world.archetypes newBundle

              liftIO $ insertEntityIntoTables newBundle world.tables archetype (entity, pointer)

class Removable c where
  removeInternal :: Proxy c -> Entity -> System ()

instance {-# OVERLAPPABLE #-} (Component c) => Removable c where
  removeInternal :: (Component c) => Proxy c -> Entity -> System ()
  removeInternal _ = removeComponentFromEntity c

instance {-# OVERLAPPING #-} (Removable c0, Removable c1) => Removable (c0, c1) where
  removeInternal :: (Removable c0, Removable c1) => Proxy (c0, c1) -> Entity -> System ()
  removeInternal _ !entity = do
    removeInternal (Proxy @c0) entity
    removeInternal (Proxy @c1) entity

remove :: forall r. (Removable r) => Entity -> System ()
remove = removeInternal (Proxy @r)

delete :: forall c. (Removable c) => ComponentResult c -> System ()
delete result = remove @c result.entity

tryGetEntityComponent :: forall c -> (Component c) => World -> Entity -> IO (Maybe c)
tryGetEntityComponent typeC world entity =
  do
    pointers <- readIORef world.entities.pointers
    components <- readIORef world.components.map

    let componentId = Map.lookup (typeRep $ Proxy @typeC) components

    case componentId of
      Nothing -> return Nothing
      Just componentId -> do
        let pointer = Map.lookup entity pointers

        case pointer of
          Nothing -> return Nothing
          Just pointer ->
            do
              pointer <- readIORef pointer
              tryGetComponentFromTables typeC world.tables pointer componentId

tryGetComponents :: forall c -> (Component c) => World -> [ArchetypeId] -> IO [ComponentResult c]
tryGetComponents typeC world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @typeC) world.components
    tryGetComponentsFromTables typeC world.tables archetypes componentId

set :: (Bundle c) => ComponentResult c -> c -> System ()
set !result !newValue = MischiefECS.World.insert newValue result.entity

modify :: (Bundle c) => ComponentResult c -> (c -> c) -> System ()
modify !result !f = MischiefECS.World.insert (f result.value) result.entity

type System = ReaderT World IO

defer :: System a -> System ()
defer !system = do
  world <- ask
  liftIO $ modifyIORef' world.deferred (++ [system $> ()])

flush :: System ()
flush = do
  world <- ask
  systems <- liftIO $ readIORef world.deferred

  for_ systems id

  liftIO $ writeIORef world.deferred []
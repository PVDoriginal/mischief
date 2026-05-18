module MischiefECS.World where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
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

data World = World
  { archetypes :: Archetypes,
    components :: Components,
    entities :: Entities,
    tables :: Tables
  }

newWorld :: IO World
newWorld = do
  archetypes <- emptyArchetypes
  components <- emptyComponents
  entities <- emptyEntities
  tables <- emptyTables

  return
    World
      { archetypes,
        components,
        entities,
        tables
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
      ProcessedBundleData
        { elements,
          archetypeId
        }

combineProcessedBundles :: World -> ProcessedBundleData -> ProcessedBundleData -> IO ProcessedBundleData
combineProcessedBundles world bundle1 bundle2 =
  let elements = Set.toList $ Set.union (Set.fromList bundle1.elements) (Set.fromList bundle2.elements)
   in do
        archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
        return
          ProcessedBundleData
            { elements,
              archetypeId
            }

removeComponentFromProcessedBundle :: World -> ComponentId -> ProcessedBundleData -> IO ProcessedBundleData
removeComponentFromProcessedBundle world componentId bundle =
  do
    let elements = Prelude.filter (\x -> x.id /= componentId) bundle.elements
    archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
    return
      ProcessedBundleData
        { elements,
          archetypeId
        }

-- | Spawn an entity in this World given a bundle of components.
spawn :: (Bundle b) => b -> System Entity
spawn bundle =
  do
    world <- ask
    bundle <- liftIO $ processBundleData world $ bundleData bundle

    let archetypeId = bundle.archetypeId

    entityIndex <- liftIO $ readIORef world.entities.counter
    liftIO $ modifyIORef world.entities.counter (+ 1)

    entityPointer <- liftIO $ insertEntityIntoTables bundle world.tables
    entityPointer <- liftIO $ newIORef entityPointer

    liftIO $ modifyIORef' world.entities.pointers $ Map.insert (Entity entityIndex) entityPointer

    return $ Entity entityIndex

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

            if empty
              then do
                liftIO $ removeArchetypeId currentPointer.archetypeId world.archetypes
              else return ()

            liftIO $ removeEntity entity world.entities

insert :: (Bundle b) => b -> Entity -> System ()
insert bundle entity =
  do
    world <- ask
    bundleData <- liftIO $ processBundleData world (bundleData bundle)
    let newComponents = Prelude.map (\x -> x.id) bundleData.elements

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

                newPointer <- liftIO $ insertEntityIntoTables newBundle world.tables
                liftIO $ writeIORef currentPointer newPointer

                let Tables tables = world.tables
                tables <- liftIO $ readIORef tables

                case Map.lookup newPointer.archetypeId tables of
                  Nothing -> undefined
                  Just newTable -> do
                    liftIO $ replaceComponentsIntoTable bundleData newPointer newTable

remove :: forall c -> (Component c) => Entity -> System ()
remove componentType entity =
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
            collectedComponents <- liftIO $ takeComponentsFromTable currentPointer currentTable
            newBundle <- liftIO $ removeComponentFromProcessedBundle world componentId collectedComponents
            newPointer <- liftIO $ insertEntityIntoTables newBundle world.tables
            liftIO $ writeIORef pointer newPointer

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

tryGetComponents :: forall c -> (Component c) => World -> [ArchetypeId] -> IO [c]
tryGetComponents typeC world archetypes =
  do
    componentId <- getComponentId (typeRep $ Proxy @typeC) world.components
    tryGetComponentsFromTables typeC world.tables archetypes componentId

type System = ReaderT World IO

module MischiefECS.World where

import Data.Typeable

import Data.Map
import Data.Map qualified as Map 

import Data.Set 
import Data.Set qualified as Set 

import Data.IORef

import MischiefECS.Entities
import MischiefECS.Components
import MischiefECS.Bundles
import MischiefECS.Tables

import Data.Proxy 
import Data.List


data World = World {
  archetypes :: Archetypes,
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

  return World {
    archetypes, 
    components, 
    entities,
    tables 
  }
    
processBundleElement :: World -> BundleElement -> IO ProcessedBundleElement
processBundleElement world BundleElement { rep, component } = 
  do
    id <- getComponentId rep world.components 
    return ProcessedBundleElement {
        id, 
        component
    } 

processBundleData :: World -> BundleData -> IO ProcessedBundleData
processBundleData world (BundleData bundleData) = 
  do 
    elements <- mapM (processBundleElement world) (Set.toList bundleData)
    archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
    return ProcessedBundleData {
      elements, 
      archetypeId 
    }

combineProcessedBundles :: World -> ProcessedBundleData -> ProcessedBundleData -> IO ProcessedBundleData
combineProcessedBundles world bundle1 bundle2 =  
  let 
    elements = Set.toList $ Set.union (Set.fromList bundle1.elements) (Set.fromList bundle2.elements) 
  in do 
    archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
    return ProcessedBundleData {
      elements,
      archetypeId 
    } 

removeComponentFromProcessedBundle :: World -> ComponentId -> ProcessedBundleData -> IO ProcessedBundleData
removeComponentFromProcessedBundle world componentId bundle = 
  do 
    let elements = Prelude.filter (\x -> x.id /= componentId) bundle.elements
    archetypeId <- getArchetypeId (Prelude.map (\x -> x.id) elements) world.archetypes
    return ProcessedBundleData {
      elements,
      archetypeId 
    } 


-- | Spawn an entity in this World given a bundle of components. 
spawnEntity :: (Bundle b) => b -> World -> IO Entity
spawnEntity bundle world = 
  do
    bundle <- processBundleData world $ bundleData bundle 

    let archetypeId = bundle.archetypeId 
    
    entityIndex <- readIORef world.entities.counter 
    modifyIORef world.entities.counter (+1) 

    entityPointer <- insertEntityIntoTables bundle world.tables
    entityPointer <- newIORef entityPointer

    modifyIORef' world.entities.pointers $ Map.insert (Entity entityIndex) entityPointer

    return $ Entity entityIndex

insertComponents :: (Bundle b) => b -> Entity -> World -> IO ()
insertComponents bundle entity world = 
  do 
    bundleData <- processBundleData world (bundleData bundle)
    let newComponents = Prelude.map (\x -> x.id) bundleData.elements

    entityPointers <- readIORef world.entities.pointers
    case Map.lookup entity entityPointers of   
      Nothing -> undefined 
      Just currentPointer -> do 

        currentPointerInternal <- readIORef currentPointer

        let Tables tables = world.tables 
        tables <- readIORef tables

        case Map.lookup currentPointerInternal.archetypeId tables of 
          Nothing -> undefined 
          Just currentTable -> do
            
            -- Simple case, no archetype change. 
            if newComponents `isSubsequenceOf` currentTable.components then 
              replaceComponentsIntoTable bundleData currentPointerInternal currentTable    
            -- Complex case, archetype change. 
            else do 
              collectedComponents <- takeComponentsFromTable currentPointerInternal currentTable 
              newBundle <- combineProcessedBundles world collectedComponents bundleData 

              newPointer <- insertEntityIntoTables newBundle world.tables  
              writeIORef currentPointer newPointer 

              case Map.lookup newPointer.archetypeId tables of 
                Nothing -> undefined 
                Just newTable -> do  
                  replaceComponentsIntoTable bundleData newPointer newTable

removeComponent :: forall c -> (Component c) => Entity -> World -> IO () 
removeComponent componentType entity world = 
  do 
    componentId <- getComponentId (typeRep $ Proxy @componentType) world.components
    entityPointers <- readIORef world.entities.pointers 

    case Map.lookup entity entityPointers of 
      Nothing -> undefined 
      Just pointer -> do 
        
        let Tables tables = world.tables
        tables <- readIORef tables 
        currentPointer <- readIORef pointer 
        
        case Map.lookup currentPointer.archetypeId tables of 
          Nothing -> undefined 
          Just currentTable -> do 
            
            collectedComponents <- takeComponentsFromTable currentPointer currentTable 
            newBundle <- removeComponentFromProcessedBundle world componentId collectedComponents 
            newPointer <- insertEntityIntoTables newBundle world.tables 
            writeIORef pointer newPointer

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


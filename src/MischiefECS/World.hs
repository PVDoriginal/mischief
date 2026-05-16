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
    
processBundleElement :: World -> BundleElement -> IO(ProcessedBundleElement)
processBundleElement world BundleElement { rep, component } = 
    do
        id <- getComponentId rep world.components 
        return ProcessedBundleElement {
            id, 
            component
        } 

processBundleData :: World -> BundleData -> IO(ProcessedBundleData)
processBundleData world (BundleData bundleData) = 
    do 
        elements <- mapM (processBundleElement world) (Set.toList bundleData)
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

        putStrLn $ show archetypeId
        putStrLn $ show (Entity entityIndex)

        entityPointer <- insertEntityIntoTables bundle world.tables

        modifyIORef' world.entities.pointers $ Map.insert (Entity entityIndex) entityPointer

        putStrLn $ show entityPointer
        putStrLn $ "\n"
        
        return $ Entity entityIndex

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
                    Just pointer -> tryGetComponentFromTables typeC world.tables pointer componentId   


module MischiefECS.Tables where 

import Data.Map
import Data.Map qualified as Map 

import MischiefECS.Components
import Data.IORef
import Data.Typeable
import MischiefECS.Entities 
import Unsafe.Coerce (unsafeCoerce)
import MischiefECS.Bundles

newtype Tables = Tables (IORef (Map ArchetypeId Table))

data Table = Table {
    components :: IORef (Map ComponentId Column),
    nRows :: IORef Integer 
}

newtype Column = Column [ErasedComponent]

emptyTables :: IO Tables 
emptyTables = do 
    map <- newIORef Map.empty 
    return $ Tables map 

newTable :: ProcessedBundleData -> IO Table 
newTable components = do 
    map <- newIORef $ Map.fromList $ Prelude.map (\x -> (x.id, Column [])) components.elements
    nRows <- newIORef 0 
    return $ Table {components=map, nRows}

insertComponentsIntoMap :: ProcessedBundleData -> Map ComponentId Column -> Map ComponentId Column 
insertComponentsIntoMap bundle map = Prelude.foldl' (\map element -> adjust (\(Column x) -> Column $ x ++ [element.component]) element.id map) map bundle.elements

insertComponentsIntoTable :: ProcessedBundleData -> Table -> IO()   
insertComponentsIntoTable bundle table = do
    modifyIORef' table.components $ insertComponentsIntoMap bundle 

insertEntityIntoTables :: ProcessedBundleData -> Tables -> IO EntityPointer 
insertEntityIntoTables bundle (Tables (tables)) = 
  do 
    innerTables <- readIORef tables
    
    -- gets the correct table (or crates another one and returns it)
    table <- case Map.lookup bundle.archetypeId innerTables of 
        Just table -> pure table
        Nothing -> do
            table <- newTable bundle
            let newTables = insert bundle.archetypeId table innerTables 
            writeIORef tables newTables
            return table 

    rowIndex <- readIORef table.nRows
    modifyIORef table.nRows (+1)

    let entityPointer = EntityPointer { archetypeId = bundle.archetypeId, rowIndex }
    
    return entityPointer 


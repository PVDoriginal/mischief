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
  columns :: IORef (Map ComponentId Column),
  nRows :: IORef Int 
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
  return $ Table {columns=map, nRows}

insertComponentsIntoMap :: ProcessedBundleData -> Map ComponentId Column -> Map ComponentId Column 
insertComponentsIntoMap bundle map = Prelude.foldl' (\map element -> adjust (\(Column x) -> Column $ x ++ [element.component]) element.id map) map bundle.elements

insertComponentsIntoTable :: ProcessedBundleData -> Table -> IO()   
insertComponentsIntoTable bundle table = do
  modifyIORef' table.columns $ insertComponentsIntoMap bundle 

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

    insertComponentsIntoTable bundle table 

    let entityPointer = EntityPointer { archetypeId = bundle.archetypeId, rowIndex }
    
    return entityPointer 

tryGetComponentFromColumn :: forall c -> (Component c) => Column -> EntityPointer -> IO(Maybe c)
tryGetComponentFromColumn typeC (Column components) pointer = 
  do 
    let element = components !! pointer.rowIndex 
    return $ tryGetComponent typeC element 


tryGetComponentFromTable :: forall c -> (Component c) => Table -> EntityPointer -> ComponentId -> IO (Maybe c)
tryGetComponentFromTable typeC table pointer componentId = 
  do 
    columns <- readIORef table.columns 
    let column = Map.lookup componentId columns 

    case column of 
      Nothing -> return Nothing 
      Just column -> tryGetComponentFromColumn typeC column pointer   


tryGetComponentFromTables :: forall c -> (Component c) => Tables -> EntityPointer -> ComponentId -> IO (Maybe c) 
tryGetComponentFromTables typeC (Tables tables) pointer componentId = 
  do 
    tables <- readIORef tables
    let table = Map.lookup pointer.archetypeId tables 

    case table of  
      Nothing -> return Nothing 
      Just table -> tryGetComponentFromTable typeC table pointer componentId

tryGetComponentsFromColumn :: forall c -> (Component c) => Column -> IO [c]
tryGetComponentsFromColumn typeC (Column components) = 
  do 
    let x = mapM (\c -> tryGetComponent typeC c) components  
    case x of 
      Nothing -> return []
      Just c -> return c  

tryGetComponentsFromTable :: forall c -> (Component c) => Table -> ComponentId -> IO [c]
tryGetComponentsFromTable typeC table componentId = 
  do 
    columns <- readIORef table.columns
    case Map.lookup componentId columns of 
      Nothing -> return []
      Just column -> tryGetComponentsFromColumn typeC column 

tryGetComponentsFromArchetype :: forall c -> (Component c) => ArchetypeId -> Map ArchetypeId Table -> ComponentId -> IO [c] 
tryGetComponentsFromArchetype typeC archetype tables componentId = 
  case Map.lookup archetype tables of 
    Nothing -> return []
    Just table -> tryGetComponentsFromTable typeC table componentId 


tryGetComponentsFromTables :: forall c -> (Component c) => Tables -> [ArchetypeId] -> ComponentId -> IO [c]
tryGetComponentsFromTables typeC (Tables tables) archetypes componentId = 
  do 
    tables <- readIORef tables 
    results <- mapM (\archetype -> tryGetComponentsFromArchetype typeC archetype tables componentId) archetypes 
    return $ concat results 
        
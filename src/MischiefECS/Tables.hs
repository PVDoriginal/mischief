module MischiefECS.Tables where 
import Data.Map
import MischiefECS.Components
import Data.IORef
import Data.Data
import MischiefECS.Entities (EntityPointer)
import Prelude hiding (lookup)

data Tables = Tables (IORef (Map ArchetypeId Table))

data Table = Table {
    components :: IORef (Map ComponentId ErasedComponentStorage)
}

data ErasedComponentStorage where
  ErasedComponentStorage ::
    (Typeable c) =>
    ComponentStorage c ->
    ErasedComponentStorage 

tryGet :: forall c -> Typeable c => ErasedComponentStorage -> Maybe (ComponentStorage c)
tryGet (type c) (ErasedComponentStorage (s :: ComponentStorage c')) = 
    case eqT @c @c' of 
        Just Refl -> Just s 
        Nothing -> Nothing 

data ComponentStorage c = ComponentStorage [c] deriving Show 

insertRow :: (Typeable c) => [c] -> Table -> IO() 
insertRow components (Table table) = do 
    innerTable <- readIORef table 
    -- do 
        -- erasedStorage <- innerTable 
    
    undefined 

newTable :: IO(Table) 
newTable = do 
    components <- newIORef empty 
    return $ Table components

insertEntity :: (Typeable a) => [a] -> ArchetypeId -> Tables -> IO(EntityPointer)
insertEntity components archetypeId (Tables (tables)) = 
  do 
    innerTables <- readIORef tables

    -- gets the correct table (or crates another one and returns it)
    Table table <- case lookup archetypeId innerTables of 
        Just table -> pure table
        Nothing -> do
            table <- newTable 
            let newTables = insert archetypeId table innerTables 
            writeIORef tables newTables
            return table 

    innerTable <- readIORef table

    undefined 


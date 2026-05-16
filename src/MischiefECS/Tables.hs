module MischiefECS.Tables where 

import Data.Map
import Data.Map qualified as Map 

import MischiefECS.Components
import Data.IORef
import Data.Typeable
import MischiefECS.Entities 
import Unsafe.Coerce (unsafeCoerce)

newtype Tables = Tables (IORef (Map ArchetypeId Table))

newtype Table = Table {
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

newtype ComponentStorage c = ComponentStorage [c] deriving Show 

unsafeCoerceStorage :: forall c -> ErasedComponentStorage -> (Typeable c, Component c) => ComponentStorage c
unsafeCoerceStorage _ erasedStorage = unsafeCoerce erasedStorage 

unsafeCoerceComponent :: forall c -> ErasedComponent -> (Typeable c, Component c) => c 
unsafeCoerceComponent _ erasedComponent = unsafeCoerce erasedComponent

unsafeInsertComponent :: forall c . (Typeable c, Component c) => ErasedComponentStorage -> ErasedComponent -> ErasedComponentStorage  
unsafeInsertComponent erasedStorage erasedComponent =
    let 
        ComponentStorage storage = unsafeCoerceStorage c erasedStorage
        component = unsafeCoerceComponent c erasedComponent 
    in 
        ErasedComponentStorage (ComponentStorage (storage ++ [component]))


newTable :: IO Table 
newTable = do 
    components <- newIORef empty 
    return $ Table components

insertEntity :: (Typeable a) => [a] -> ArchetypeId -> Tables -> IO EntityPointer 
insertEntity components archetypeId (Tables (tables)) = 
  do 
    innerTables <- readIORef tables

    -- gets the correct table (or crates another one and returns it)
    Table table <- case Map.lookup archetypeId innerTables of 
        Just table -> pure table
        Nothing -> do
            table <- newTable 
            let newTables = insert archetypeId table innerTables 
            writeIORef tables newTables
            return table 

    innerTable <- readIORef table

    undefined 


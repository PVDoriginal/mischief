module MischiefECS.Entities where 
import Data.Map
import Data.IORef
import MischiefECS.Components

data Entity = Entity {
    id :: Integer 
} deriving (Show, Eq, Ord) 

data EntityPointer = EntityPointer {
    archetypeId :: ArchetypeId, 
    rowIndex :: Integer 
} deriving Show 

data Entities = Entities {
    pointers :: IORef(Map Entity EntityPointer),
    counter :: IORef Integer
}

emptyEntities :: IO Entities 
emptyEntities = do 
    map <- newIORef empty
    counter <- newIORef 0 
    return $ Entities map counter 

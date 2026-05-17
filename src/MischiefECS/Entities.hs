module MischiefECS.Entities where 
import Data.Map
import Data.IORef
import MischiefECS.Components

data Entity = Entity {
  id :: Int 
} deriving (Show, Eq, Ord) 

data EntityPointer = EntityPointer {
  archetypeId :: ArchetypeId, 
  rowIndex :: Int 
} deriving Show 

data Entities = Entities {
  pointers :: IORef(Map Entity EntityPointer),
  counter :: IORef Int
} 

emptyEntities :: IO Entities 
emptyEntities = do 
  map <- newIORef empty
  counter <- newIORef 0 
  return $ Entities map counter 

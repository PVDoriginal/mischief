module MischiefECS.Entities where 
import Data.Map
import Data.IORef

data Entity = Entity {
    id :: Integer 
} deriving (Show, Eq, Ord) 

data EntityPointer = EntityPointer {
    archetype_index :: Integer, 
    row_index :: Integer 
} deriving Show 

data Entities = Entities {
    pointers :: IORef(Map Entity EntityPointer),
    counter :: IORef(Integer)
}

emptyEntities :: IO(Entities) 
emptyEntities = do 
    map <- newIORef empty
    counter <- newIORef 0 
    return $ Entities map counter 

-- getArchetypeId :: Entity -> Entities -> IO(EntityPointer)
-- getArchetypeId t Entities {pointers, counter} = do 
--     pointers <- readIORef pointers 
    
--     case lookup t pointers  of 
--         Just t -> return t 
--         Nothing -> undefined 

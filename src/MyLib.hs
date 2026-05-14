module MyLib  where

import Data.Data
import Data.Map
import Data.IORef

data World = World {
    archetypes :: IORef(Map Integer ArchetypeStorage), 
    entities :: IORef(Map Entity EntityPointer),
    entityCounter :: IORef Integer 
} 

incrementEntityCounter :: World -> IO()
incrementEntityCounter World { entityCounter } = modifyIORef entityCounter (+1)

newWorld :: IO(World)
newWorld = do  
    archetypes <- newIORef empty 
    entities <- newIORef empty 
    entityCounter <- newIORef 0
    let world = World archetypes entities entityCounter
    return world 

createEmptyEntity :: World -> IO(Entity) 
createEmptyEntity World {archetypes, entities, entityCounter } = do 
    undefined 

data Entity = Entity {
    id :: Integer 
} deriving Show 

data EntityPointer = EntityPointer {
    archetype_index :: Integer, 
    row_index :: Integer 
} deriving Show 

data Archetype = Archetype {
    id :: Integer
} deriving Show 

data ArchetypeStorage = ArchetypeStorage {
    hash :: Integer, 
    components :: Map Integer ErasedComponentStorage
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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data X = X Int deriving Show 
test = tryGet (type X) storage
  where storage = ErasedComponentStorage (ComponentStorage [X 5])
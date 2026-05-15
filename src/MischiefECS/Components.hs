module MischiefECS.Components where 

import Data.Map
import Data.Kind
import Data.Data (Typeable, TypeRep)
import Data.IORef
import Prelude hiding (lookup)
import Data.Typeable
import Data.List

data ComponentId = ComponentId {
    id :: Integer
} deriving (Show, Eq, Ord) 

data Components = Components { map :: IORef (Map TypeRep ComponentId), counter :: IORef(Integer) }  

emptyComponents :: IO(Components) 
emptyComponents = do 
    map <- newIORef empty
    counter <- newIORef 0 
    return $ Components map counter 

getComponentId :: TypeRep -> Components -> IO(ComponentId)
getComponentId t Components {map, counter} = do 
    map <- readIORef map 
    
    case Data.Map.lookup t map  of 
        Just t -> return t

    result <- readIORef counter 
    modifyIORef counter (+1) 
    return $ ComponentId result 

data ArchetypeId = ArchetypeId {
    id :: Integer
} deriving (Show, Eq, Ord) 

data Archetypes = Archetypes { map :: IORef (Map [ComponentId] ArchetypeId), counter :: IORef(Integer) }

emptyArchetypes :: IO(Archetypes) 
emptyArchetypes = do 
    map <- newIORef empty
    counter <- newIORef 0 
    return $ Archetypes map counter 

getArchetypeId :: [ComponentId] -> Archetypes -> IO(ArchetypeId)
getArchetypeId t Archetypes {map, counter} = do 
    map <- readIORef map 
    
    case Data.Map.lookup t map  of 
        Just t -> return t

    result <- readIORef counter 
    modifyIORef counter (+1) 
    return $ ArchetypeId result 

data ErasedComponent where
  ErasedComponent :: (Typeable c) => c -> ErasedComponent 

class Typeable c => Component c where 
  erase :: c -> ErasedComponent
  erase = ErasedComponent 

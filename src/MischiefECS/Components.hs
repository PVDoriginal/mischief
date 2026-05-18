module MischiefECS.Components where 


import Data.Typeable
import Data.IORef

import Data.Map
import Data.Map qualified as Map 

import Data.List
import Data.List qualified as List 

import Data.Set 
import Data.Set qualified as Set 

newtype ComponentId = ComponentId {
  id :: Int
} deriving (Show, Eq, Ord) 

data Components = Components { map :: IORef (Map TypeRep ComponentId), counter :: IORef Int }  

emptyComponents :: IO Components
emptyComponents = do 
  map <- newIORef Map.empty
  counter <- newIORef 0 
  return $ Components map counter 

getComponentId :: TypeRep -> Components -> IO ComponentId
getComponentId t Components {map, counter} = do 
  innerMap <- readIORef map 
  
  case Data.Map.lookup t innerMap  of 
    Just t -> return t
    Nothing -> do 
      result <- readIORef counter 
      modifyIORef counter (+1) 

      modifyIORef map $ Data.Map.insert t (ComponentId result)  
      
      return $ ComponentId result 


tryGetComponent :: forall c -> Component c => ErasedComponent -> Maybe c
tryGetComponent (type c) (ErasedComponent (s :: c')) = 
  case eqT @c @c' of
    Just Refl -> Just s 
    Nothing -> Nothing 

newtype ArchetypeId = ArchetypeId {
  id :: Int
} deriving (Show, Eq, Ord) 

data Archetypes = Archetypes { map :: IORef (Map [ComponentId] ArchetypeId), counter :: IORef Int }

emptyArchetypes :: IO Archetypes 
emptyArchetypes = do 
  map <- newIORef Map.empty
  counter <- newIORef 0 
  return $ Archetypes map counter 

-- | Get the archetype ID from a list of component IDs. 
getArchetypeId :: [ComponentId] -> Archetypes -> IO ArchetypeId
getArchetypeId t Archetypes {map, counter} = do 
  innerMap <- readIORef map 

  case Data.Map.lookup t innerMap  of 
    Just t -> return t
    Nothing -> do 
      result <- readIORef counter 
      modifyIORef counter (+1) 

      modifyIORef map $ Data.Map.insert t (ArchetypeId result)

      return $ ArchetypeId result 

removeArchetypeId :: ArchetypeId -> Archetypes -> IO ()
removeArchetypeId id archetypes = do 
  modifyIORef' archetypes.map (Map.filter (\v -> v /= id))

findMatchingArchetypes :: [ComponentId] -> Archetypes -> IO [ArchetypeId]
findMatchingArchetypes components archetypes = 
  do
    archetypes <- readIORef archetypes.map 
    let archetypesList = Map.toList archetypes 
    return $ List.map snd $ List.filter (\(archetype, _) -> components `isSubsequenceOf` archetype) archetypesList 
 
data ErasedComponent where
  ErasedComponent :: (Typeable c) => c -> ErasedComponent 

class Typeable c => Component c where 
  erase :: c -> ErasedComponent
  erase = ErasedComponent 

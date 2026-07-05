module MischiefECS.Entities
  ( EntityPointer (..),
    decreaseRowIndex,
    Entities,
    EntityCounter,
    getNewEntity,
    removeEntity,
    getPointer,
    insertPointer,
    emptyEntities,
    Entity (Entity, id),
    isAliveIO,
  )
where

import Control.Concurrent.STM.TVar
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import GHC.Conc
import MischiefECS.Components
import MischiefECS.Entities.Internal

data EntityPointer = EntityPointer
  { archetypeId :: ArchetypeId,
    rowIndex :: Int
  }
  deriving (Show)

decreaseRowIndex :: EntityPointer -> EntityPointer
decreaseRowIndex EntityPointer {archetypeId, rowIndex} = EntityPointer {archetypeId, rowIndex = rowIndex - 1}

data Entities = Entities
  { pointers :: IORef (Map Entity (IORef EntityPointer)),
    counter :: TVar EntityCounter
  }

data EntityCounter = EntityCounter {counter :: Int, free :: [Entity]}

insertPointer :: Entity -> IORef EntityPointer -> Entities -> IO ()
insertPointer entity pointer entities = do
  modifyIORef' entities.pointers (Map.insert entity pointer)

getPointer :: Entity -> Entities -> IO (Maybe (IORef EntityPointer))
getPointer entity entities = do
  pointers <- readIORef entities.pointers
  return $ Map.lookup entity pointers

getNewEntity :: Entities -> IO Entity
getNewEntity entities = atomically $ do
  EntityCounter {counter, free} <- readTVar entities.counter
  case free of
    [] -> do
      writeTVar entities.counter EntityCounter {counter = counter + 1, free}
      return $ Entity {id = counter, gen = 1}
    (Entity {id, gen} : xs) -> do
      writeTVar entities.counter EntityCounter {counter = counter, free = xs}
      return $ Entity {id, gen = gen + 1}

removeEntity :: Entity -> Entities -> IO ()
removeEntity entity entities = do
  modifyIORef' entities.pointers (Map.delete entity)
  atomically $ do
    EntityCounter {counter, free} <- readTVar entities.counter
    writeTVar entities.counter EntityCounter {counter, free = entity : free}

emptyEntities :: IO Entities
emptyEntities = do
  map <- newIORef Map.empty
  counter <- newTVarIO EntityCounter {counter = 1, free = []}
  return $ Entities map counter

isAliveIO :: Entity -> Entities -> IO Bool
isAliveIO entity Entities {pointers} = do
  pointers <- readIORef pointers
  return $ isJust $ Map.lookup entity pointers

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
  )
where

import Control.Concurrent.STM.TVar
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
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

data EntityCounter = EntityCounter {counter :: Int, free :: [Entity], generations :: TVar (Map Int Int)}

insertPointer :: Entity -> IORef EntityPointer -> Entities -> IO ()
insertPointer entity pointer entities = do
  modifyIORef' entities.pointers (Map.insert entity pointer)

getPointer :: Entity -> Entities -> IO (Maybe (IORef EntityPointer))
getPointer entity entities = do
  pointers <- readIORef entities.pointers
  if entity.gen /= 0
    then return $ Map.lookup entity pointers
    else do
      gen <- atomically $ do
        counter <- readTVar entities.counter
        generations <- readTVar counter.generations
        return (Map.lookup entity.id generations)

      return $ do
        gen' <- gen
        Map.lookup (Entity {id = entity.id, gen = gen'}) pointers

getNewEntity :: Entities -> IO Entity
getNewEntity entities = atomically $ do
  EntityCounter {counter, free, generations} <- readTVar entities.counter
  case free of
    [] -> do
      modifyTVar' generations (Map.insert counter 1)
      writeTVar entities.counter EntityCounter {counter = counter + 1, free, generations}
      return $ Entity {id = counter, gen = 1}
    (Entity {id, gen} : xs) -> do
      modifyTVar' generations (Map.insert counter (gen + 1))
      writeTVar entities.counter EntityCounter {counter = counter, free = xs, generations}
      return $ Entity {id, gen = gen + 1}

removeEntity :: Entity -> Entities -> IO ()
removeEntity entity entities = do
  modifyIORef' entities.pointers (Map.delete entity)
  atomically $ do
    EntityCounter {counter, free, generations} <- readTVar entities.counter
    modifyTVar' generations (Map.delete entity.id)
    writeTVar entities.counter EntityCounter {counter, free = entity : free, generations}

emptyEntities :: IO Entities
emptyEntities = do
  map <- newIORef Map.empty
  generations <- newTVarIO Map.empty
  counter <- newTVarIO EntityCounter {counter = 1, free = [], generations}
  return $ Entities map counter

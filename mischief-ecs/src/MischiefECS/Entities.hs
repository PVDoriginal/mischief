module MischiefECS.Entities
  ( EntityPointer (..),
    decreaseRowIndex,
    Entities (..),
    EntityCounter,
    getNewEntity,
    removeEntity,
    emptyEntities,
    Entity,
  )
where

import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
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

getNewEntity :: Entities -> IO Entity
getNewEntity entities = atomically $ do
  EntityCounter {counter, free} <- readTVar entities.counter
  case free of
    [] -> do
      writeTVar entities.counter EntityCounter {counter = counter + 1, free}
      return $ Entity {id = counter, gen = 0}
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

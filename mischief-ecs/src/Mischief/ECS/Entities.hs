-- {-# LANGUAGE DeriveLift #-}

module Mischief.ECS.Entities
  ( -- * Entity
    Entity (..),
    EntityPointer (..),
    getPointer,
    isAliveIO,

    -- * Storage
    Entities,
    EntityCounter,
    getNewEntity,
    removeEntity,
    insertPointer,
    emptyEntities,
  )
where

import Control.Concurrent.STM.TVar
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import GHC.Base
import GHC.Conc
import Mischief.ECS.Components
import Mischief.ECS.EntityDef

-- | A pointer to the exact table and row that an entity is in.
--
-- The first @Int#@ is the id of its archetype. The second is the index of the row it's in.
data EntityPointer = EntityPointer (# Int#, Int# #)

-- { archetypeId :: ArchetypeId,
--   rowIndex :: Int
-- }
-- deriving (Show)

-- | A storage for entity ids and pointers.
data Entities = Entities
  { -- | Associates each @Entity@ to an @EntityPointer@.
    pointers :: IORef (Map Entity (IORef EntityPointer)),
    -- | A counter for assigning new entity ids. It is in a TVar so
    -- it can be used in parallel systems.
    counter :: TVar EntityCounter
  }

-- | A counter and a list for recycling entities.
data EntityCounter = EntityCounter {counter :: Word#, free :: [Entity]}

-- | Associate an Entity with a new pointer.
insertPointer :: Entity -> IORef EntityPointer -> Entities -> IO ()
insertPointer entity pointer entities = do
  modifyIORef' entities.pointers (Map.insert entity pointer)

-- | Get the pointer to an entity.
getPointer :: Entity -> Entities -> IO (Maybe (IORef EntityPointer))
getPointer entity entities = do
  pointers <- readIORef entities.pointers
  return $ Map.lookup entity pointers

-- | Create a new entity.
getNewEntity :: Entities -> IO Entity
getNewEntity entities = atomically $ do
  EntityCounter {counter, free} <- readTVar entities.counter
  case free of
    [] -> do
      writeTVar entities.counter EntityCounter {counter = plusWord# counter 1##, free}
      return $ Entity (# counter, 1## #)
    (Entity (# id, gen #) : xs) -> do
      writeTVar entities.counter EntityCounter {counter = counter, free = xs}
      return $ Entity (# id, plusWord# gen 1## #)

-- | Remove an entity from storage.
removeEntity :: Entity -> Entities -> IO ()
removeEntity entity entities = do
  modifyIORef' entities.pointers (Map.delete entity)
  atomically $ do
    EntityCounter {counter, free} <- readTVar entities.counter
    writeTVar entities.counter EntityCounter {counter, free = entity : free}

-- | Create a new storage for entities.
emptyEntities :: IO Entities
emptyEntities = do
  map <- newIORef Map.empty
  counter <- newTVarIO EntityCounter {counter = 1##, free = []}
  return $ Entities map counter

-- | Check if an Entity is alive through IO.
isAliveIO :: Entity -> Entities -> IO Bool
isAliveIO entity Entities {pointers} = do
  pointers <- readIORef pointers
  return $ isJust $ Map.lookup entity pointers

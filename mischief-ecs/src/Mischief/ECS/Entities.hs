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
    getNewEntityComp,
    emptyEntities,
  )
where

import Control.Concurrent.STM.TVar
import Control.Monad
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import GHC.Base
import GHC.Conc
import Mischief.ECS.Components
import Mischief.ECS.EntityDef
import Mischief.ECS.Vec (IOVec)
import Mischief.ECS.Vec qualified as Vec

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
    pointers :: IOVec (IORef EntityPointer),
    nullPtr :: IORef EntityPointer,
    -- | A counter for assigning new entity ids. It is in a TVar so
    -- it can be used in parallel systems.
    counter :: TVar EntityCounter
  }

-- | A counter and a list for recycling entities.
data EntityCounter = EntityCounter {counter :: Word#, free :: [Entity]}

-- | Associate an Entity with a new pointer.
insertPointer :: Entity -> IORef EntityPointer -> Entities -> IO ()
insertPointer (Entity (# i, _ #)) pointer entities = do
  Vec.write entities.pointers (I# $ word2Int# i) pointer

-- | Get the pointer to an entity.
getPointer :: Entity -> Entities -> IO (Maybe (IORef EntityPointer))
getPointer (Entity (# i', _ #)) entities = do
  let i = I# (word2Int# i')
  l <- Vec.length entities.pointers

  if i < l
    then do
      p <- Vec.unsafeRead entities.pointers i
      pure $ if p /= entities.nullPtr then Just p else Nothing
    else
      pure Nothing

-- | Create a new entity.
getNewEntity :: Entities -> IO Entity
getNewEntity entities = do
  (e, reused) <- atomically $ do
    EntityCounter {counter, free} <- readTVar entities.counter
    case free of
      [] -> do
        writeTVar entities.counter EntityCounter {counter = plusWord# counter 1##, free}
        pure (Entity (# counter, 1## #), False)
      (Entity (# id, gen #) : xs) -> do
        writeTVar entities.counter EntityCounter {counter = counter, free = xs}
        pure (Entity (# id, plusWord# gen 1## #), True)

  unless reused $ do
    Vec.pushBack entities.pointers entities.nullPtr

  pure e

-- | Creates a new entity using a fresh id, never incrementing the generation of a previous one.
getNewEntityComp :: Entities -> IO Entity
getNewEntityComp entities = do
  Vec.pushBack entities.pointers entities.nullPtr
  atomically $ do
    EntityCounter {counter, free} <- readTVar entities.counter
    let e = Entity (# counter, 0## #)
    writeTVar entities.counter EntityCounter {counter = plusWord# counter 1##, free}
    pure e

-- | Remove an entity from storage.
removeEntity :: Entity -> Entities -> IO ()
removeEntity (Entity (# i', g #)) entities = do
  let i = I# (word2Int# i')
  l <- Vec.length entities.pointers

  when (i < l) $ do
    Vec.unsafeWrite entities.pointers i entities.nullPtr

    atomically $ do
      EntityCounter {counter, free} <- readTVar entities.counter
      writeTVar entities.counter EntityCounter {counter, free = Entity (# i', g #) : free}

-- | Create a new storage for entities.
emptyEntities :: IO Entities
emptyEntities = do
  map <- Vec.new 256
  counter <- newTVarIO EntityCounter {counter = 1##, free = []}
  nullPtr <- newIORef $ EntityPointer (# 0#, 0# #)

  Vec.pushBack map nullPtr
  return $ Entities map nullPtr counter

-- | Check if an Entity is alive through IO.
isAliveIO :: Entity -> Entities -> IO Bool
isAliveIO (Entity (# i', _ #)) entities = do
  let i = I# (word2Int# i')
  l <- Vec.length entities.pointers

  if i < l
    then do
      p <- Vec.unsafeRead entities.pointers i
      pure $ p /= entities.nullPtr
    else pure False

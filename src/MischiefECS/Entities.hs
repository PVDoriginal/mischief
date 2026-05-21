module MischiefECS.Entities where

import Data.IORef
import Data.Map
import Data.Map qualified as Map
import MischiefECS.Components

newtype Entity = Entity {id :: Int} deriving (Show, Eq, Ord)

instance Component Entity

data EntityPointer = EntityPointer
  { archetypeId :: ArchetypeId
  , rowIndex :: Int
  }
  deriving (Show)

decreaseRowIndex :: EntityPointer -> EntityPointer
decreaseRowIndex EntityPointer{archetypeId, rowIndex} = EntityPointer{archetypeId, rowIndex = rowIndex - 1}

data Entities = Entities
  { pointers :: IORef (Map Entity (IORef EntityPointer))
  , counter :: IORef Int
  }

removeEntity :: Entity -> Entities -> IO ()
removeEntity entity entities = do
  modifyIORef' entities.pointers (Map.delete entity)

emptyEntities :: IO Entities
emptyEntities = do
  map <- newIORef empty
  counter <- newIORef 0
  return $ Entities map counter

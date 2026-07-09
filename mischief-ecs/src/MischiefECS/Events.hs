module MischiefECS.Events where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..))
import Data.Data (Typeable)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.List
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query
import MischiefECS.World.Query.Queryable

data EventProxy e = EventProxy deriving (Component, Queryable)

newtype Observer e = Observer (e -> System ()) deriving anyclass (Component, Queryable)

newtype ObserverOrder = ObserverOrder Int
  deriving (Show)
  deriving anyclass (Component, Queryable)
  deriving newtype (Eq, Ord)

-- | @Event@ typeclass.
class (Typeable e) => Event e where
  eraseEvent :: e -> ErasedEvent
  eraseEvent = ErasedEvent

trigger :: (Event e) => e -> System ()
trigger event = do
  world <- unsafeGetWorld
  liftIO $ modifyIORef' world.events (++ [eraseEvent event])

flushEvents :: System ()
flushEvents = do
  world <- unsafeGetWorld
  events <- liftIO $ readIORef world.events
  for_ events runEvent
  liftIO $ writeIORef world.events []

runEvent :: ErasedEvent -> System ()
runEvent (ErasedEvent (event :: e)) = do
  observers' <- query @(Observer e, EventProxy e, ObserverOrder)
  let observers = sortBy (\(_, _, a) (_, _, b) -> compare a b) observers'
  for_ observers $ \(observer, _, _) -> do
    let Observer f = value observer
    f event

newtype OnInsert c = OnInsert {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

data OnInsertRel c = OnInsertRel {entity :: Entity, target :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

newtype OnRemove c = OnRemove {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

data ErasedEvent where
  ErasedEvent :: (Typeable e) => e -> ErasedEvent

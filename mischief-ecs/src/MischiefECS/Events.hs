module MischiefECS.Events where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Data (Typeable)
import Data.Foldable (for_)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.Events.Internal
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Query

data EventProxy e = EventProxy deriving (Component, Queryable)

class (Typeable e) => Event e where
  eraseEvent :: e -> ErasedEvent
  eraseEvent = ErasedEvent

newtype Observer e = Observer (e -> System ()) deriving (Component, Queryable)

spawnObserver :: forall e. (Event e) => Observer e -> System ()
spawnObserver observer = do
  _ <- spawn (observer, EventProxy @e)
  return ()

trigger :: (Event e) => e -> System ()
trigger event = do
  world <- ask
  liftIO $ modifyIORef' world.events (++ [eraseEvent event])

runEvent :: ErasedEvent -> System ()
runEvent (ErasedEvent (event :: e)) = do
  observers <- query @(Observer e, EventProxy e)
  for_ observers $ \(observer, _) -> do
    let Observer f = observer.value
    f event

flushEvents :: System ()
flushEvents = do
  world <- ask
  events <- liftIO $ readIORef world.events
  for_ events runEvent
  liftIO $ writeIORef world.events []

newtype OnAdd c = OnAdd Entity deriving (Event)

newtype OnInsert c = OnInsert Entity deriving (Event)

newtype OnRemove c = OnRemove Entity deriving (Event)
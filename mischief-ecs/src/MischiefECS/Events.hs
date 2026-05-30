module MischiefECS.Events where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Data (Typeable)
import MischiefECS.App
import MischiefECS.Components
import MischiefECS.Events.Internal
import MischiefECS.World (System, spawn)

data EventProxy e = EventProxy

class (Typeable e) => Event e where
  eraseEvent :: e -> ErasedEvent
  eraseEvent = ErasedEvent

instance (Event e) => Component (EventProxy e)

newtype Observer e = Observer (e -> System ()) deriving (Component)

addObserver :: (Event e) => (e -> System ()) -> Plugin ()
addObserver observer = do
  app <- ask
  liftIO $ runReaderT (spawnObserver $ Observer observer) app.world

spawnObserver :: forall e. (Event e) => Observer e -> System ()
spawnObserver observer = do
  _ <- spawn (observer, EventProxy @e)
  return ()

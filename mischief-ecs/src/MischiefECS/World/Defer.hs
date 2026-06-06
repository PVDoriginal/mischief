module MischiefECS.World.Defer where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor
import Data.IORef
import MischiefECS.World
import MischiefECS.World.Internal

-- class Defer s where
--   -- | Defer a command to be ran after the current 'System' is finished,
--   -- or when 'flush' is called.
--   defer :: System a -> s

-- instance Defer (System ()) where
--   defer :: System a -> System ()
--   defer !system = do
--     world <- ask
--     liftIO $ modifyIORef' world.deferred (++ [system $> ()])

-- instance Defer (ParSystem ()) where
--   defer :: System a -> ParSystem ()
--   defer !system = do
--     ParWorld {deferred} <- ask
--     liftIO $ modifyIORef' deferred (++ [system $> ()])

class Defer m where
  defer :: System a -> m ()

instance Defer System where
  defer :: System a -> System ()
  defer !s = do
    world <- ask
    liftIO $ modifyIORef' world.deferred (++ [s $> ()])

instance Defer ParSystem where
  defer :: System a -> ParSystem ()
  defer !s = do
    ParWorld {deferred} <- ask
    liftIO $ modifyIORef' deferred (++ [s $> ()])

-- | Flush the current list of deferred commands.
flush :: System ()
flush = do
  world <- ask
  systems <- liftIO $ readIORef world.deferred

  sequence_ systems

  liftIO $ writeIORef world.deferred []

module MischiefECS.World.Defer where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
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

flushAsync :: System ()
flushAsync = do
  world <- ask

  systems <- liftIO $ atomically $ do
    systems <- readTVar world.deferredAsync
    writeTVar world.deferredAsync []
    return systems

  sequence_ systems

forkDefer :: System a -> System a
forkDefer s = do
  world <- ask
  deferred <- liftIO $ newIORef []

  let world' = setDeferred deferred world
  a <- liftIO $ runSystem s world'

  deferred <- liftIO $ readIORef deferred
  liftIO $ modifyIORef' world.deferred (++ deferred)
  return a

-- let world' =

-- forkSystem :: ParSystem () -> System ()
-- forkSystem (ParSystem !x) = do
--   world <- ask
--   _ <- liftIO $ forkIO $ do
--     deferred <- newIORef []
--     runReaderT x ParWorld {world, deferred}
--     deferred' <- readIORef deferred

--     atomically $ modifyTVar' world.deferredAsync (++ deferred')

--   return ()

runAfter :: (MonadSystem w m) => IO a -> (a -> System ()) -> m ()
runAfter !function !system = do
  world <- asks getWorld
  _ <- liftIO $ forkIO $ do
    a <- function
    atomically $ modifyTVar' world.deferredAsync (++ [system a])

  return ()

delay :: (MonadSystem w m) => Int -> System () -> m ()
delay !d system = runAfter (threadDelay d) (const system)
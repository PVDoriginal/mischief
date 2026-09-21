module Mischief.ECS.World.Defer where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable
import Data.Functor
import Data.IORef
import Mischief.ECS.World

class Defer m where
  defer :: System a -> m ()

instance Defer System where
  defer :: System a -> System ()
  defer !s = do
    world <- unsafeGetWorld
    liftIO $ modifyIORef' world.deferred (++ [s $> ()])

instance Defer ParSystem where
  defer :: System a -> ParSystem ()
  defer !s = do
    ParWorld {parDeferred} <- ask
    liftIO $ modifyIORef' parDeferred (++ [s $> ()])

-- | Flush the current list of deferred commands.
flush :: System ()
flush = do
  world <- unsafeGetWorld
  systems <- liftIO $ readIORef world.deferred

  for_ systems $ \s -> do
    forkDefer $ do
      s
      flush

  liftIO $ writeIORef world.deferred []

flushAsync :: System ()
flushAsync = do
  world <- unsafeGetWorld

  systems <- liftIO $ atomically $ do
    systems <- readTVar world.deferredAsync
    writeTVar world.deferredAsync []
    return systems

  for_ systems $ \s -> do
    forkDefer $ do
      s
      flush

forkDefer :: System a -> System a
forkDefer s = do
  world <- unsafeGetWorld
  deferred <- liftIO $ newIORef []

  let world' = setDeferred deferred world
  a <- liftIO $ runSystem s world'

  deferred <- liftIO $ readIORef deferred
  liftIO $ modifyIORef' world.deferred (++ deferred)
  return a

runAfter :: (MonadSystem w m) => IO a -> (a -> System ()) -> m ()
runAfter !function !system = do
  world <- unsafeGetWorld
  _ <- liftIO $ forkIO $ do
    a <- function
    atomically $ modifyTVar' world.deferredAsync (++ [system a])

  return ()

delay :: (MonadSystem w m) => Int -> System () -> m ()
delay !d system = runAfter (threadDelay d) (const system)

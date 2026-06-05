module MischiefECS.World.Par where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Traversable
import GHC.Conc (numCapabilities)
import MischiefECS.World
import MischiefECS.World.Internal

par :: (MonadSystem w m) => [ParSystem ()] -> m ()
par !parSystems = do
  world <- asks getWorld

  x <- forM parSystems $ \(ParSystem p) -> do
    systems <- liftIO $ newIORef []
    id <- liftIO $ async $ runReaderT p ParWorld {world, deferred = systems}
    -- liftIO $ print "started a thread!"
    return (id, systems)

  for_ x $ \(id, systems) -> do
    systems <- liftIO $ readIORef systems
    liftIO $ wait id
    liftIO $ modifyIORef' world.deferred (++ systems)

parIterList :: (MonadSystem w m) => [a] -> ([a] -> ParSystem ()) -> m ()
parIterList list s = do
  world <- asks getWorld

  let n = numCapabilities
  let len = length list

  let chunks = group (n `div` len) list

  x <- forM chunks $ \chunk -> do
    systems <- liftIO $ newIORef []
    let ParSystem p = s chunk
    id <- liftIO $ async $ runReaderT p ParWorld {world, deferred = systems}
    return (id, systems)

  for_ x $ \(id, systems) -> do
    systems <- liftIO $ readIORef systems
    liftIO $ wait id
    liftIO $ modifyIORef' world.deferred (++ systems)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l = take n l : group n (drop n l)

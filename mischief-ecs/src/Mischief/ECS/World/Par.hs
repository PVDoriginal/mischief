module Mischief.ECS.World.Par where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Traversable
import GHC.Conc (numCapabilities)
import Mischief.ECS.Hidden
import Mischief.ECS.World

par :: (MonadSystem w m) => [ParSystem ()] -> m ()
par !parSystems = do
  world <- unsafeGetWorld

  x <- forM parSystems $ \(ParSystem p) -> do
    systems <- liftIO $ newIORef []
    id <- liftIO $ async $ runReaderT p ParWorld {world = Hidden world, parDeferred = systems}
    return (id, systems)

  for_ x $ \(id, systems) -> do
    liftIO $ wait id
    systems <- liftIO $ readIORef systems
    liftIO $ modifyIORef' world.deferred (++ systems)

parIterList :: (MonadSystem w m, Foldable t) => t a -> ([a] -> ParSystem b) -> m [b]
parIterList !list !s = do
  world <- unsafeGetWorld

  let n = numCapabilities
  let len = length list

  let chunks = group (len `div` n) (toList list)

  x <- forM chunks $ \chunk -> do
    systems <- liftIO $ newIORef []
    let ParSystem p = s chunk
    id <- liftIO $ async $ runReaderT p ParWorld {world = Hidden world, parDeferred = systems}
    return (id, systems)

  for x $ \(id, systems) -> do
    a <- liftIO $ wait id
    systems <- liftIO $ readIORef systems
    liftIO $ modifyIORef' world.deferred (++ systems)
    return a

group :: Int -> [a] -> [[a]]
group _ [] = []
group 0 l = [l]
group !n !l = take n l : group n (drop n l)

parIter :: (MonadSystem w m, Foldable t) => t a -> (a -> ParSystem b) -> m [b]
parIter x s = concat <$> parIterList x (`for` s)

parIter_ :: (MonadSystem w m, Foldable t) => t a -> (a -> ParSystem b) -> m ()
parIter_ x s = void (parIterList x (`for_` s))

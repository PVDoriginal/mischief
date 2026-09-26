module Mischief.ECS.World.Par
  ( par,
    par',
    parIterList,
    parFor,
    parFor_,
    parFor',
    parFor'_,
  )
where

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
import Mischief.ECS.World.Defer

-- | Receives a list of @ParSystems@ and runs them in parallel. Flushes the deferred queue when done!
par :: [ParSystem ()] -> System ()
par x = do
  par' x
  flush

-- | Receives a list of @ParSystems@ and runs them in parallel. Doesn't flush the deferred queue when done!
par' :: (MonadSystem w m) => [ParSystem ()] -> m ()
par' !parSystems = do
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

-- | Iterates over a list in parallel. Flushes the deferred queue when done.
parFor :: (Foldable t) => t a -> (a -> ParSystem b) -> System [b]
parFor x s = do
  a <- parFor' x s
  flush
  pure a

-- | Iterates over a list in parallel and discards the results. Flushes the deffered queue when done.
parFor_ :: (Foldable t) => t a -> (a -> ParSystem b) -> System ()
parFor_ x s = do
  void $ parFor' x s
  flush

-- | Iterates over a list in parallel. Doesn't flush the deferred queue when done.
parFor' :: (MonadSystem w m, Foldable t) => t a -> (a -> ParSystem b) -> m [b]
parFor' x s = concat <$> parIterList x (`for` s)

-- | Iterates over a list in parallel and discards the results. Doesn't flush the deffered queue when done.
parFor'_ :: (MonadSystem w m, Foldable t) => t a -> (a -> ParSystem b) -> m ()
parFor'_ x s = void (parIterList x (`for_` s))

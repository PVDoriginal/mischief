module Mischief.ECS.Interval (start, stop, Interval) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Mischief.ECS.World

newtype Interval = Interval (IORef Bool)

start :: (MonadSystem w m) => Int -> System () -> m Interval
start t s = do
  world <- unsafeGetWorld
  i <- Interval <$> liftIO (newIORef False)
  _ <- liftIO $ async $ run world i t s
  return i

run :: World -> Interval -> Int -> System () -> IO ()
run world (Interval break) t s = do
  b <- liftIO $ readIORef break
  unless b $ do
    liftIO $ threadDelay t

    liftIO $ atomically $ modifyTVar' world.deferredAsync (++ [s])
    run world (Interval break) t s

stop :: (MonadSystem w m) => Interval -> m ()
stop (Interval break) = liftIO $ writeIORef break True
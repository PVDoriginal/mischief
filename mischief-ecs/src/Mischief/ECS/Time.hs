module Mischief.ECS.Time where

import Control.Monad.IO.Class
import Mischief.ECS.App.Plugins
import Mischief.ECS.App.Schedules
import Mischief.ECS.Components
import Mischief.ECS.Resources
import Mischief.ECS.Systems
import Mischief.ECS.Utils
import Mischief.ECS.World
import System.Clock

class ToTime t where
  toTime :: t -> Time

data Time = Time
  { delta :: TimeSpec,
    elapsed :: TimeSpec
  }

data VirtualTime = VirtualTime
  { virtualDelta :: TimeSpec,
    virtualElapsed :: TimeSpec
  }
  deriving (Show, Component)

instance ToTime VirtualTime where
  toTime v = Time {delta = v.virtualDelta, elapsed = v.virtualElapsed}

time :: System Time
time = toTime . unwrap <$> res @VirtualTime

delta :: System Float
delta = do
  time <- time
  pure $ specToSecs time.delta

elapsed :: System Float
elapsed = do
  time <- time
  pure $ specToSecs time.elapsed

specToSecs :: TimeSpec -> Float
specToSecs a = fromIntegral a.sec + fromIntegral a.nsec / 1000000000

data TimePlugin

instance Plugin TimePlugin where
  init = do
    currentTime <- liftIO $ getTime Monotonic
    insertRes $ VirtualTime {virtualDelta = TimeSpec {sec = 0, nsec = 0}, virtualElapsed = currentTime}
    schedule First (systems updateTime)

updateTime :: System ()
updateTime = do
  Just time <- res @VirtualTime
  currentTime <- liftIO $ getTime Monotonic
  insertRes VirtualTime {virtualDelta = currentTime - time.virtualElapsed, virtualElapsed = currentTime}

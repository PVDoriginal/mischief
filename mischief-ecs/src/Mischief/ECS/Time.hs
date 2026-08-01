module Mischief.ECS.Time where

import Control.Monad.IO.Class
import GHC.Records (HasField (getField))
import Mischief.ECS.App
import Mischief.ECS.App.Plugins
import Mischief.ECS.App.Schedules
import Mischief.ECS.Components
import Mischief.ECS.Resources
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Modify
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Queryable
import System.Clock

data Time = Time
  { delta :: TimeSpec,
    elapsed :: TimeSpec
  }
  deriving (Show, Component)

instance HasField "deltaSecs" Time Float where
  getField :: Time -> Float
  getField Time {delta} = fromIntegral delta.sec + fromIntegral delta.nsec / 1000000000

data TimePlugin = TimePlugin deriving (Eq)

instance Plugin TimePlugin where
  init _ = do
    currentTime <- liftIO $ getTime Monotonic
    insertRes $ Time {delta = TimeSpec {sec = 0, nsec = 0}, elapsed = currentTime}
    Systems.add First updateTime

updateTime :: System ()
updateTime = do
  Just time <- res @Time
  currentTime <- liftIO $ getTime Monotonic
  insertRes Time {delta = currentTime - time.elapsed, elapsed = currentTime}

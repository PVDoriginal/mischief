module MischiefECS.Time where

import Control.Monad.IO.Class
import GHC.Records (HasField (getField))
import MischiefECS.App
import MischiefECS.App.Plugins
import MischiefECS.App.Schedules
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Modify
import MischiefECS.World.Query
import MischiefECS.World.Query.Queryable
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
    addSystems First updateTime

updateTime :: System ()
updateTime = do
  Just time <- res @Time
  currentTime <- liftIO $ getTime Monotonic
  modify time $ \Time {elapsed} -> Time {delta = currentTime - elapsed, elapsed = currentTime}

module MischiefECS.Time where

import Control.Monad.IO.Class
import GHC.Records (HasField (getField))
import MischiefECS.App
import MischiefECS.App.Schedules
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Query
import MischiefECS.World.Query.Queryable
import System.Clock

data Time = Time
  { delta :: TimeSpec,
    elapsed :: TimeSpec
  }
  deriving (Show, Component, Queryable)

instance HasField "deltaSecs" Time Float where
  getField :: Time -> Float
  getField Time {delta} = fromIntegral delta.sec + fromIntegral delta.nsec / 1000000000

timePlugin :: Plugin ()
timePlugin = do
  run $ addSystems First updateTime

updateTime :: System ()
updateTime = do
  time <- res @Time
  currentTime <- liftIO $ getTime Monotonic

  case time of
    Nothing ->
      insertRes $ Time {delta = TimeSpec {sec = 0, nsec = 0}, elapsed = currentTime}
    Just time ->
      insertRes $ Time {delta = currentTime - time.elapsed, elapsed = currentTime}

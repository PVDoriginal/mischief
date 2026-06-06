module MischiefECS.Time where

import Control.Monad.IO.Class
import MischiefECS.App
import MischiefECS.App.Schedules
import MischiefECS.Components
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Query
import System.Clock

data Time = Time
  { delta :: TimeSpec,
    elapsed :: TimeSpec
  }
  deriving (Show, Queryable)

instance Component Time where
  type Storage Time = ResourceStorage

timePlugin :: Plugin ()
timePlugin = do
  addSystems First updateTime

updateTime :: System ()
updateTime = do
  time <- single @Time
  currentTime <- liftIO $ getTime Monotonic

  case time of
    Nothing ->
      insertResource $ Time {delta = TimeSpec {sec = 0, nsec = 0}, elapsed = currentTime}
    Just time ->
      insertResource $ Time {delta = currentTime - time.value.elapsed, elapsed = currentTime}
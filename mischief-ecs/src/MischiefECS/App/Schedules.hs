module MischiefECS.App.Schedules where

import Data.Data
import Data.Map (Map)
import Data.Map qualified as Map
import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.World
import MischiefECS.World.Query
import MischiefECS.World.Spawn

newtype ScheduleLabel = ScheduleLabel {rep :: TypeRep}
  deriving stock (Eq, Ord, Show)
  deriving anyclass (Component)

class (Typeable s) => Schedule s

data Init = Init deriving (Schedule)

data PreStartup = PreStartup deriving (Schedule)

data Startup = Startup deriving (Schedule)

data PostStartup = PostStartup deriving (Schedule)

data First = First deriving (Schedule)

data Update = Update deriving (Schedule)

data PreUpdate = PreUpdate deriving (Schedule)

data PostUpdate = PostUpdate deriving (Schedule)

newtype ScheduleId = ScheduleId {id :: Entity}

newtype Schedules = Schedules {inner :: Map TypeRep ScheduleId} deriving anyclass (Component)

getScheduleId :: (Schedule sch) => sch -> System ScheduleId
getScheduleId sch = do
  Just schedules <- res @Schedules
  case Map.lookup (typeOf sch) schedules.inner of
    Just x -> return x
    Nothing -> do
      e <- spawn (ScheduleLabel (typeOf sch))
      return $ ScheduleId e

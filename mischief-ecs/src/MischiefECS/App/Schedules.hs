module MischiefECS.App.Schedules where

import Data.Data
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
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

data StartupSchedule = StartupSchedule deriving (Component)

data UpdateSchedule = UpdateSchedule deriving (Component)

newtype ScheduleId = ScheduleId {id :: Entity}

newtype Schedules = Schedules {inner :: Map TypeRep ScheduleId}
  deriving stock (Generic)
  deriving anyclass (Component, Default)

getScheduleId :: ScheduleLabel -> System ScheduleId
getScheduleId sch = do
  Just schedules <- res @Schedules
  case Map.lookup sch.rep schedules.inner of
    Just x -> return x
    Nothing -> do
      e <- spawn sch
      return $ ScheduleId e

scheduleEntity :: (Schedule sch) => sch -> System Entity
scheduleEntity sch = do
  ScheduleId id <- getScheduleId $ ScheduleLabel $ typeOf sch
  return id

-- runSystemsIn :: (Schedule sch) => sch -> System ()

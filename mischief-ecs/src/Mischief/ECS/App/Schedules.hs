module Mischief.ECS.App.Schedules where

import Data.Data
import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Log
import Mischief.ECS.Resources
import Mischief.ECS.World
import Mischief.ECS.World.Modify
import Mischief.ECS.World.Query
import Mischief.ECS.World.Spawn

newtype ScheduleLabel = ScheduleLabel {rep :: TypeRep}
  deriving stock (Eq, Ord, Show)
  deriving anyclass (Component)

class (Typeable s) => Schedule s

data Init = Init deriving (Schedule, Show)

data PreStartup = PreStartup deriving (Schedule)

data Startup = Startup deriving (Schedule, Show)

data PostStartup = PostStartup deriving (Schedule)

data First = First deriving (Schedule, Show)

data Update = Update deriving (Schedule, Show)

data PreUpdate = PreUpdate deriving (Schedule)

data PostUpdate = PostUpdate deriving (Schedule)

data StartupSchedule = StartupSchedule deriving (Component)

data UpdateSchedule = UpdateSchedule deriving (Component)

newtype ScheduleId = ScheduleId {id :: Entity} deriving (Ord, Eq, Show)

newtype Schedules = Schedules {inner :: Map TypeRep ScheduleId}
  deriving stock (Generic)
  deriving anyclass (Component, Default)

getScheduleId :: ScheduleLabel -> System ScheduleId
getScheduleId sch = do
  Just schedules <- res @Schedules
  warn $ text sch
  case Map.lookup sch.rep schedules.inner of
    Just x -> return x
    Nothing -> do
      e <- spawn sch
      insertRes $ Schedules $ Map.insert sch.rep (ScheduleId e) schedules.inner
      return $ ScheduleId e

scheduleEntity :: (Schedule sch) => sch -> System Entity
scheduleEntity sch = do
  ScheduleId id <- getScheduleId $ ScheduleLabel $ typeOf sch
  warn $ text id
  return id

-- runSystemsIn :: (Schedule sch) => sch -> System ()

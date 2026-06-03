module MischiefECS.App.Scheduler where

import Data.Data
import Data.Map (Map)
import MischiefECS.Graph (Graph)
import MischiefECS.Graph qualified as Graph
import MischiefECS.World

data Scheduler = Scheduler
  { schedules :: Graph ScheduleLabel,
    systems :: Map ScheduleLabel (Graph SystemSetId)
  }

newtype SystemSetId = SystemSetId Int

newtype ScheduleLabel = ScheduleLabel {rep :: TypeRep} deriving (Eq, Ord, Show)

data ScheduleGraph = Undefined1

class (Typeable s) => Schedule s

data Startup = Startup deriving (Schedule)

data Update = Update deriving (Schedule)

data PreUpdate = PreUpdate deriving (Schedule)

data PostUpdate = PostUpdate deriving (Schedule)

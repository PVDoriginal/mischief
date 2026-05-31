module MischiefECS.App.Scheduler where

import Data.Data
import MischiefECS.World

data Scheduler = Undefined

data ScheduleGraph = Undefined1

class (Typeable s) => Schedule s

data Startup = Startup deriving (Schedule)

data Update = Update deriving (Schedule)

data PreUpdate = PreUpdate deriving (Schedule)

data PostUpdate = PostUpdate deriving (Schedule)

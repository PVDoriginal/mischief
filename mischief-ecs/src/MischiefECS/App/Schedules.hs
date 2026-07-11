module MischiefECS.App.Schedules where

import Data.Data

newtype ScheduleLabel = ScheduleLabel {rep :: TypeRep} deriving (Eq, Ord, Show)

class (Typeable s) => Schedule s

data Init = Init deriving (Schedule)

data PreStartup = PreStartup deriving (Schedule)

data Startup = Startup deriving (Schedule)

data PostStartup = PostStartup deriving (Schedule)

data First = First deriving (Schedule)

data Update = Update deriving (Schedule)

data PreUpdate = PreUpdate deriving (Schedule)

data PostUpdate = PostUpdate deriving (Schedule)

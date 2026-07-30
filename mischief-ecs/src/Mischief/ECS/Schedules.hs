module Mischief.ECS.Schedules where

import Mischief.ECS.App
import Mischief.ECS.App.Schedules
import Mischief.ECS.Entities
import Mischief.ECS.World

get :: (Schedule sc) => sc -> System Entity
get = scheduleEntity

run :: (Schedule sc) => sc -> System ()
run = runSchedule
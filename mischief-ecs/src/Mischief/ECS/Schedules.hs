{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.Schedules where

import Language.Haskell.TH (Extension (AllowAmbiguousTypes))
import Mischief.ECS.App
import Mischief.ECS.App.Schedules
import Mischief.ECS.Entities
import Mischief.ECS.World

get :: forall sc. (Schedule sc) => System Entity
get = scheduleEntity @sc

run :: forall sc. (Schedule sc) => System ()
run = runSchedule @sc
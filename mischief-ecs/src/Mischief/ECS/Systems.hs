-- |
-- Module with utility functions for creating
-- and scheduling systems.
module Mischief.ECS.Systems where

import Data.Foldable
import Data.Kind
import GHC.Stack.Types
import Mischief.ECS.App.Schedules
import Mischief.ECS.App.SystemConfig
import Mischief.ECS.App.Systems (ScheduledIn (ScheduledIn), SystemFunction (SystemFunction), removeSystemFromMap, systemEntity)
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Mappable
import Mischief.ECS.Relationships.Order
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Spawn qualified as Spawn
import Mischief.ECS.World.Utils

add :: (HasCallStack, Schedule sc, SystemConfig s) => sc -> s -> System ()
add schedule system = do
  let SystemConfigData {systems, edges} = systemConfigData system

  for_ systems $ \system -> systemEntity schedule system

  for_ edges $ \(s1, s2) -> do
    id1 <- systemEntity schedule s1
    id2 <- systemEntity schedule s2
    insert (Rel (Before, id2)) id1

remove :: (Schedule sc, Collectable a [System ()]) => sc -> a -> System ()
remove schedule systems = do
  let y :: [System ()] = collect systems
  for_ y (removeOne schedule)

removeOne :: (Schedule sc) => sc -> System () -> System ()
removeOne schedule system = do
  s <- meta schedule system
  sch <- scheduleEntity schedule
  removeSystemFromMap (ScheduleId sch) system

  despawn s

  query' (C @Entity) (WithR @Before s)
    >>= traverse_ (removeRel @Before s)

spawn :: System () -> System Entity
spawn = Spawn.spawn . SystemFunction

meta :: (Schedule sc) => sc -> System () -> System Entity
meta = systemEntity

schedule :: (Schedule sc) => sc -> System () -> System ()
schedule sch s = do
  s' <- meta sch s
  sch' <- scheduleEntity sch

  insert (Rel (ScheduledIn, sch')) s'

unschedule :: (Schedule sc) => sc -> System () -> System ()
unschedule sch s = do
  s' <- meta sch s
  sch' <- scheduleEntity sch

  removeRel @ScheduledIn sch' s'

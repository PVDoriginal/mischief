-- |
-- Module with utility functions for creating
-- and scheduling systems.
module Mischief.ECS.Systems where

import Control.Monad.IO.Class
import Control.Monad.Reader (runReaderT)
import Data.Foldable
import Data.Kind
import GHC.Stack.Types
import Mischief.ECS.App.Schedules
import Mischief.ECS.App.SystemConfig
import Mischief.ECS.App.Systems (ScheduledIn (ScheduledIn), SystemFunction (SystemFunction), removeSystemFromMap, systemEntity)
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Hidden
import Mischief.ECS.Mappable
import Mischief.ECS.Relationships.Order
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Spawn qualified as Spawn
import Mischief.ECS.World.Utils

data SystemConfig = SystemConfig
  { systems :: [System ()],
    edges :: [(System (), System ())]
  }

newtype Systems = Systems [System ()] deriving newtype (Semigroup)

instance EraseIntoStorage (System ()) Systems where
  erase a = Systems [a]

type ToSystems a = Collectable a Systems

systems :: (ToSystems a) => a -> SystemConfig
systems a =
  let Systems systems = collect a
   in SystemConfig {systems, edges = []}

after :: (ToSystems a) => a -> SystemConfig -> SystemConfig
after a s =
  let Systems systems = collect a
   in SystemConfig {systems = s.systems, edges = s.edges ++ zip systems s.systems}

before :: (ToSystems a) => a -> SystemConfig -> SystemConfig
before a s =
  let Systems systems = collect a
   in SystemConfig {systems = s.systems, edges = s.edges ++ zip s.systems systems}

schedule :: (Schedule sc) => sc -> SystemConfig -> System ()
schedule schedule SystemConfig {systems, edges} = do
  for_ systems $ \system -> systemEntity schedule system

  for_ edges $ \(s1, s2) -> do
    id1 <- systemEntity schedule s1
    id2 <- systemEntity schedule s2
    insert (Rel Before id2) id1

remove :: (Schedule sc, ToSystems a) => sc -> a -> System ()
remove schedule systems = do
  sch <- scheduleEntity schedule

  let Systems y = collect systems
  for_ y $ \system -> do
    s <- Mischief.ECS.Systems.get schedule system
    removeSystemFromMap (ScheduleId sch) system

    despawn s

    query (mkQuery' E (With (R @Before s)))
      >>= traverse_ (removeRel @Before s)

order :: (Schedule sc, ToSystems a, ToSystems b) => sc -> (a, b) -> System ()
order schedule (s1, s2) = do
  let Systems systems1 = collect s1
  let Systems systems2 = collect s2

  for_ systems1 $ \s1 -> for_ systems2 $ \s2 -> do
    s1 <- Mischief.ECS.Systems.get schedule s1
    s2 <- Mischief.ECS.Systems.get schedule s2

    insert (Rel Before s2) s1

spawn :: System () -> System Entity
spawn = Spawn.spawn . SystemFunction

get :: (Schedule sc) => sc -> System () -> System Entity
get = systemEntity

-- schedule :: (Schedule sc) => sc -> System () -> System ()
-- schedule sch s = do
--   s' <- Mischief.ECS.Systems.get sch s
--   sch' <- scheduleEntity sch

--   insert (Rel ScheduledIn sch') s'

unschedule :: (Schedule sc, ToSystems a) => sc -> a -> System ()
unschedule sch s = do
  sch' <- scheduleEntity sch

  let Systems systems = collect s
  for_ systems $ \s -> do
    s' <- Mischief.ECS.Systems.get sch s

    removeRel @ScheduledIn sch' s'

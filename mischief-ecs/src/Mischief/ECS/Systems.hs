{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Module with utility functions for creating
-- and scheduling systems.
module Mischief.ECS.Systems where

import Data.Foldable
import Language.Haskell.TH (Extension (AllowAmbiguousTypes))
import Mischief.ECS.App.Schedules
import Mischief.ECS.App.Systems (ScheduledIn, SystemFunction (SystemFunction), removeSystemFromMap, systemEntity)
import Mischief.ECS.Collectable
import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Relationships.Order
import Mischief.ECS.World
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Spawn qualified as Spawn

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

schedule :: forall sc. (Schedule sc) => SystemConfig -> System ()
schedule SystemConfig {systems, edges} = do
  for_ systems $ \system -> systemEntity @sc system

  for_ edges $ \(s1, s2) -> do
    id1 <- systemEntity @sc s1
    id2 <- systemEntity @sc s2
    insert (Rel Before id2) id1

order :: forall sc a b. (Schedule sc, ToSystems a, ToSystems b) => (a, b) -> System ()
order (s1, s2) = do
  let Systems systems1 = collect s1
  let Systems systems2 = collect s2

  for_ systems1 $ \s1 -> for_ systems2 $ \s2 -> do
    s1 <- Mischief.ECS.Systems.get @sc s1
    s2 <- Mischief.ECS.Systems.get @sc s2

    insert (Rel Before s2) s1

spawn :: System () -> System Entity
spawn = Spawn.spawn . SystemFunction

get :: forall sc. (Schedule sc) => System () -> System Entity
get = systemEntity @sc

unschedule :: forall sc a. (Schedule sc, ToSystems a) => a -> System ()
unschedule s = do
  sch' <- scheduleEntity @sc

  let Systems systems = collect s
  for_ systems $ \s -> do
    s' <- Mischief.ECS.Systems.get @sc s

    removeRel @ScheduledIn sch' s'

unschedule' :: forall sc a. (Schedule sc, ToSystems a) => a -> System ()
unschedule' systems = do
  sch <- scheduleEntity @sc

  let Systems y = collect systems
  for_ y $ \system -> do
    s <- Mischief.ECS.Systems.get @sc system
    removeSystemFromMap (ScheduleId sch) system

    despawn s

    query (mkQuery' E (With (R @Before s)))
      >>= traverse_ (removeRel @Before s)
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: Monadic Queries]("Mischief.ECS.Tutorial.Guides.MonadicQueries")
--
-- [Next Chapter: Keeping Track of Time]("Mischief.ECS.Tutorial.Guides.Time")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Scheduling
  ( -- * Learn You an ECS for Great Mischief! - 2.7. Scheduling and Running Systems
    -- $intro

    -- * Automatic Scheduling
    -- $auto_scheduling

    -- * Manual Scheduling
    -- $manual_scheduling

    -- * Creating and Running Schedules
    -- $schedules

    -- * Deferring Systems
    -- $defer

    -- * [Next Chapter: Keeping Track of Time]("Mischief.ECS.Tutorial.Guides.Time")
  )
where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Traversable (for)
import Mischief.ECS

-- $intro
-- A @System@ in Mischief is a Monad that executes operations on a World.
--
-- Unlike other ECS's, systems here are fully composable, In fact, most of the functions discussed in this tutorial so far were systems.
-- For instance, the type of @spawn@ is:
--
-- @
-- 'spawn' :: ('Bundle' b) => b -> 'System' ()@
-- @
--
-- Systems can either be ran directly, or they can be added scheduled.

-- $auto_scheduling
-- In order to schedule a system (or a tuple of systems) you can just use @systems@ to put your hand on them, and @schedule@ to add it to a given Schedule.
--
-- @
-- 'systems' (foo, bar)
--   & 'schedule' \@SomeSchedule
-- @
--
-- Systems scheduled this way are stores as a combination of the actual system and the schedule.
-- If we were to add @foo@ to /another/ schedule, it would be considered a different system.
--
-- The @aftr@ and @before@ functions can be used to explicitly add ordering with other systems when registering them:
--
-- @
-- 'systems' foo
--   & 'before' foo
--   & 'after' baz
--   & 'schedule' \@SomeSchedule
-- @
--
--
-- A system that's been scheduled this way can be unscheduled with @unschedule@ or @unschedule'@.
--
-- @unschedule@ will keep the system alive but disconnect it from the given schedule. If re-scheduled later,
-- the system will keep its previous ordering and other configuration:
--
-- @
-- 'unschedule' \@SomeSchedule foo
-- @
--
-- @unschedule'@ will completely wipe a system, erasing all of its configuration. If re-scheduled later,
-- the system will be completely new and it won't have its previous ordering.
--
-- @
-- 'unschedule'' \@SomeSchedule foo
-- @

-- $manual_scheduling
-- In order to create a scheduled system you can just spawn an entity for it:
--
-- @
-- foo :: 'System' ()
-- @
--
-- @
-- fooEntity <- Systems.'Mischief.ECS.Systems.spawn' foo
-- @
--
-- Where:
--
-- @
-- import "Mischief.ECS.Systems" qualified as Systems
-- @
--
-- You can get a schedule's entity using the following function:
--
-- @
-- scheduleEntity <- Schedules.'Mischief.ECS.Schedules.get' \@SomeSchedule
-- @
--
-- To add the system to the schedule you just insert a @ScheduledIn@ relationship between the two:
--
-- @
-- 'insert' ('Rel' 'ScheduledIn' scheduleEntity) fooEntity
-- @
--
-- This will make make your system run along with @SomeSchedule@. Compared to using @Systems.add@, this method will not do any sort of bookkeeping for you.
-- Is is your job to keep track of the spawned system's entity.
--
-- Two systems can be ordered by using the @Before@ relationship:
--
-- @
-- fooEntity <- Systems.'Mischief.ECS.Systems.spawn' foo
-- barEntity <- Systems.'Mischief.ECS.Systems.spawn' bar
--
-- 'insert' ('Rel' 'Before' fooEntity) barEntity
-- @
--
-- The above will order @bar@ to happen before @foo@.

-- $schedules
-- Same as systems, @Schedules@ are entities. Each schedule has an associated type:
--
-- @
-- data Update deriving ('Schedule')
-- @
--
-- You can both create and get the the entity of a schedule using @Schedules.get@:
--
-- @
-- update <- Schedules.'Mischief.ECS.Schedules.get' \@Update
-- @
--
-- You can run a schedule using @Schedules.run@:
--
-- @
-- Schedules.'Mischief.ECS.Schedules.run' \@Update
-- @
--
-- This will run all systems currently linked to that Schedule, respecting their ordering.
--
-- Mischief has two components: @'StartupSchedule'@ and @'UpdateSchedule'@ which you can add to a schedule to make it automatically run on app startup, respectively each frame.
--
-- Schedules can also be ordered via @'Before'@ (same relationship used for ordering systems).

-- $defer
-- @defer@ is a very important primitive.
--
-- Normally every systems has its effect applied immediately. When you write @insert (Name \"Bob\") bob@,
-- you are /immediately/ inserting the respective component. When you do @e <- 'spawn' ()@, you are /immediately/ spawning that entity into the World.
--
-- @'defer'@ takes a system and stores it in an internal queue instead of directly applying it.
--
-- @
-- 'defer' $ 'spawn' ()
-- @
--
-- @
-- 'defer' $ do
--   e <- 'spawn' ()
--   'insert' ('Name' \"Name\") e
-- @
--
-- The queue of deferred systems will be flushed (running all the systems in it) at a sync point, usually in-between systems.
--
-- You can also flush the queue manually using @flush@.

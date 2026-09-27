{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: (Docs) Queries]("Mischief.ECS.Tutorial.Documentation.Queries")
--
-- [Next Chapter: (Docs) Events and Messages]("Mischief.ECS.Tutorial.Documentation.Events")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Documentation.Systems
  ( -- * Learn You an ECS for Great Mischief! - 3.4. (Docs) Systems

    -- * Automatic Scheduling
    -- $auto_scheduling

    -- * Manual Scheduling
    -- $manual_scheduling

    -- * Schedules
    -- $schedules

    -- * [Next Chapter: (Docs) Events and Messages]("Mischief.ECS.Tutorial.Documentation.Events")
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Data.Foldable (for_)
import Mischief.ECS

-- $auto_scheduling
-- TODO

-- $manual_scheduling
-- TODO

-- $schedules
-- @Schedules.get@ retrieves the entity of a schedule, or spawns it if it's not registered.
--
-- @
-- update <- Schedules.'Mischief.ECS.Schedules.get' Update
-- @
--
-- @Schedules.run@ runs a schedule:
--
-- @
-- Schedules.'Mischief.ECS.Schedules.run' \@Update
-- @
--
-- Inserting @'StartupSchedule'@ on a schedule will make it run in the startup loop.
--
-- Inserting @'UpdateSchedule'@ on a schedule will make it run in the update loop.
--
-- @'Before'@ can be added between two such schedules to order them.
--
-- The systems Mischief has by default in Startup:
--
-- * 'PreStartup'
-- * 'Startup'
-- * 'PostStartup'
--
-- And in Update:
--
-- * 'First'
-- * 'PreUpdate'
-- * 'Update'
-- * 'PostUpdate'
-- * 'Last'

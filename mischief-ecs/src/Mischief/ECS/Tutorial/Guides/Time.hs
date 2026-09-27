{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Queries Tutorial
-- Description: How To Mischief.
--
-- [Previous Chapter: Scheduling and Running Systems]("Mischief.ECS.Tutorial.Guides.Scheduling")
--
-- [Next Chapter: Parallelism and Asynchronicity]("Mischief.ECS.Tutorial.Guides.Parallelism")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Time
  ( -- * Learn You an ECS for Great Mischief! - 2.8. Keeping Track of Time
    -- $intro

    -- * Time
    -- $time

    -- * Timer
    -- $timer

    -- * [Next Chapter: Parallelism and Asynchronicity]("Mischief.ECS.Tutorial.Guides.Parallelism")
  )
where

import Control.Monad (void)
import Data.Foldable (for_)
import Data.Traversable (for)
import Mischief.ECS
import System.Clock (TimeSpec)

-- $intro
-- This chapter shows you how to get the Time and use Timers.

-- $time
-- To read time you can use the functions from:
--
-- @
-- import "Mischief.ECS.Time" qualified as Time
-- @
--
-- The most important one is @delta@, which gets you the time that has passed since the last frame, in seconds.
--
-- @
-- d <- Time.'Mischief.ECS.Time.delta'
-- @
--
-- You can also use @Time.time@ to get the actual Time:
--
-- @
-- t <- Time.'Mischief.ECS.Time.time'
-- @
--
-- @Time@ has two fields: @.elapsed@ and @.delta@. Both contain a 'TimeSpec'.
--
-- You can convert a TimeSpec to seconds using @specToSecs@:
--
-- @
-- let elapsed = Time.'Mischief.ECS.Time.specToSecs' t.elapsed
-- let delta = Time.'Mischief.ECS.Time.specToSecs' t.delta
-- @

-- $timer
-- A Timer is a data type that can keep track of time and be ticked down each frame.
--
-- @
-- import "Mischief.ECS.Timer" qualified as Timer
-- @
--
-- There are two modes for a timer. A @Repeat@ timer will infinitely loop around once it finished, while an @Once@ timer stops after finishing once.
--
-- To create a timer you must give it a duration and a mode:
--
-- @
-- let timer = Timer.'Mischief.ECS.Timer.new' 0.5 Timer.'Mischief.ECS.Timer.Repeat'
-- @
--
-- The @tick@ function takes an amount of time (typically obtained from @Time.delta@) and returns a new timer, along with a bool specifying whether it just finished or not:
--
-- @
-- delta <- Time.'Mischief.ECS.Time.delta'
-- let (timer', justFinished) <- Timer.'Mischief.ECS.Timer.tick' delta timer
-- @

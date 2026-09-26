-- |
-- Module: Queries Tutorial
-- Description: How To Mischief.
--
-- [Previous Chapter: Keeping Track of Time]("Mischief.ECS.Tutorial.Guides.Time")
--
-- [Next Chapter: Events and Messages]("Mischief.ECS.Tutorial.Guides.Events")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Parallelism
  ( -- * Learn You an ECS for Great Mischief! - 2.9. Parallelism and Asynchronicity
    -- $intro

    -- * The System Monads
    -- $monad

    -- * Running Systems in Parallel
    -- $par

    -- * Iterating in Parallel
    -- $parFor

    -- * Asynchronicity
    -- $async

    -- * [Next Chapter: Events and Messages]("Mischief.ECS.Tutorial.Guides.Events")
  )
where

-- $intro
-- Mischief offers a few primitives for making systems run in parallel or asynchronously.

-- $monad
-- There is another monad that acts similary to @System@. It is called @ParSystem@, and
-- it doesn't allow for immediate mutations of the World.
--
-- Both @ParSystem@ and @System@ instance the @MonadSystem@ class, which you can use to write functions that work with both:
--
-- @
-- f :: (MonadSystem w m) => m ()
-- @
--
-- Functions such as @query@, @query_@, @single@ work with both monads, since they only read data and don't write.
--
-- Functions that mutate the world only work with @System@, requiring you to use @defer@ (see previous chapter) when used in @ParSystem@:
--
-- @
-- 'defer' $ 'insert' (Health 100) bob
-- @

-- $par
-- You can run a list of @ParSystesm@ in parallel using the @par@ function:
--
-- @
-- par [foo, bar]
-- @

-- $parFor
-- You can iterate over Foldables in parallel using @parFor@:
--
-- @
-- names <- 'query' ['q'|Entity, Name|]
-- 'parFor' names $ \\(entity, Name name) -> do
--   'defer' $ 'insert' (Name $ name ++ \"2\") entity
-- @

-- $async
-- Mischief has a couple of primitived that allow running systems asynchronously.
--
-- === runAfter
--
-- Asynchonicity in Mischief can be achieved using the @runAfter@ primtitive.
-- You provide it an 'IO' action that returns an @a@, and a system which consumes that @a@.
--
-- The 'IO' will be ran fully asychrnously and then will add the system to a special async-friendly deferred list that
-- will be applied at the first available sync point.
--
-- We can look at @delay@ as an example of how this may be useful:
--
-- @
-- 'delay' d system = 'runAfter' ('threadDelay' d) ('const' system)
-- @
--
-- Whcih allows delaying any system by an amount of time:
--
-- @
-- 'delay' 500 $ 'insert' (Health 100) player
-- @
--
-- === Intervals
--
-- Intervals are another way of running async systems.
--
-- @
-- import "Mischief.ECS.Interval" qualified as Interval
-- @
--
-- This allows you to set a system to repeatedly run on a fixed interval:
--
--
-- This will print \"Hello!\" once per second:
--
-- @
-- hello <- Interval.'Mischief.ECS.Interval.start' 1000 ('info' \"Hello!\")
-- @
--
-- The interval can be stopped at any time by calling @Interval.stop@ on the returned object:
--
-- @
-- Interval.'Mischief.ECS.Interval.stop' hello
-- @

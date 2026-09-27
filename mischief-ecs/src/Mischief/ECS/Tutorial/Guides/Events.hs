{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Queries Tutorial
-- Description: How To Mischief.
--
-- [Previous Chapter: Parallelism and Asynchronicity]("Mischief.ECS.Tutorial.Guides.Parallelism")
--
-- [Next Chapter: Change Detection]("Mischief.ECS.Tutorial.Guides.Change")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Events
  ( -- * Learn You an ECS for Great Mischief! - 2.10. Events and Messages
    -- $intro

    -- * Events
    -- $events

    -- * Messages
    -- $msg

    -- * [Next Chapter: Change Detection]("Mischief.ECS.Tutorial.Guides.Change")
  )
where

import Mischief.ECS

-- $intro
-- This chapter presents two ways of communicating between systems. @Events@ and @Messages@!.

-- $events
-- An event is any type deriving the @Event@ typeclass.
--
-- @
-- data Foo = Foo 'Int' deriving ('Event')
-- @
--
-- Events can be triggered using @trigger@:
--
-- @
-- 'trigger' (Foo 5)
-- @
--
-- Events can be listened to by spawning an observer. Observers will be called immediately when an event is triggered.
--
-- @
-- listenToFoo :: Foo -> 'System' ()
-- listenToFoo foo = ...
-- @
--
-- @
-- _ <- 'spawn' ('Observer' listenToFoo)
-- @

-- $msg
-- @Messages@ are just convenient wrappers around resources that are used for inter-system communication.
--
-- A message is a type that derives the @Message@ typeclass:
--
-- @
-- data Foo = Foo 'Int' deriving ('Message')
-- @
--
-- Tools for working with messages are found provided by the @Messages@ module:
--
-- @
-- import "Mischief.ECS.Messages" qualified as Messages
-- @
--
-- There are two main functions used to work with messages, @write@ and @read@.
--
-- You can use @write@ to write a new message into the buffer:
--
-- @
-- Messages.'Mischief.ECS.Messages.write' (Foo 5)
-- @
--
-- And you can use @read@ to drain the buffer of a certain type of message:
--
-- @
-- messages <- Messages.'Mischief.ECS.Messages.read' \@Foo
-- @
--
-- @
-- messages :: [Foo]
-- @
--
-- @read@ will get all messages that haven't yet been read by the current system, erasing them from the buffer.
--
-- Messages are better than events when dealing with higher throughput, since they natively function in batches, letting you process multiple messages at a time from
-- within the same system.

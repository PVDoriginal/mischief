{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Events and Messages Tutorial
-- Description: Tutorial on using @Events and Messages@
--
-- This module contains a more in-depth tutorial on using @Events and Messages@.
--
-- [Previous Chapter: Systems]("Mischief.ECS.Tutorial.Systems")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Events
  ( -- * Learn You an ECS for Great Mischief! - 8. Events and Messages

    -- * Introduction
    -- $intro

    -- * Events
    -- $events

    -- * Messages
    -- $msg
  )
where

import Mischief.ECS

-- $intro
-- This chapter presents two ways of communicating between systems. @Events@ and @Messages@!.

-- $events
-- You should already be pretty familiar with @Events@ if you've read the rest of this tutorial. But let's get into them again just in case.
--
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
-- _ <- [Observers]("Mischief.ECS.Observers").'Mischief.ECS.Observers.spawn' listenToFoo
-- @
--
-- There are also pre-defined systems called by Mischief:
--
-- * @'OnInsert' c@ - called after @c@ has been inserted on an entity.
-- * @'OnRemove' c@ - called before @c@ is removed from an entity.
-- * @'OnInsertRel' c@ / @'OnRemoveRel' c@ - same as the above but for relationships.
--
-- Events are really handy, but they can be become inefficient if called many times in a frame, since there will be an individual system ran per triggered event.
-- That's where messages come in!

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
-- import "Mischief.ECS.Messages" qualified as [Messages]("Mischief.ECS.Messages")
-- @
--
-- There are two main functions used to deal with messages, @write@ and @read@.
--
-- You can use @write@ to write a new message into the buffer:
--
-- @
-- [Messages]("Mischief.ECS.Messages").'Mischief.ECS.Messages.write' (Foo 5)
-- @
--
-- And you can use @read@ to drain the buffer of a certain type of message:
--
-- @
-- messages <- [Messages]("Mischief.ECS.Messages").'Mischief.ECS.Messages.read' \@Foo
-- @
--
-- @
-- messages :: [Foo]
-- @
--
-- @read@ will get all messages that haven't yet been read by the current system, erasing them from the buffer.
--
-- Messages are better than events for higher throughput, since they function in batches so a system can process multiple messages at a time.

{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
--
-- [Previous Chapter: (Docs) Systems]("Mischief.ECS.Tutorial.Documentation.Systems")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Documentation.Events
  ( -- * Learn You an ECS for Great Mischief! - 3.5. (Docs) Events and Messages

    -- * Events
    -- $events

    -- * Messages
    -- $msg
  )
where

import Mischief.ECS

-- $events
-- Instance @Event@ on a type to make it an event:
--
-- @
-- data Foo = Foo deriving ('Event')
-- @
--
-- @trigger@ triggers an Event:
--
-- @
-- 'trigger' (Foo 5)
-- @
--
-- An event @e@ can be listened to with a system like:
--
-- @
-- sysName :: e -> 'System' ()
-- @
--
-- An observer to listen to an event can be spawned like so:
--
-- @
-- _ <- 'spawn' $ 'Observer' sysName
-- @
--
-- Pre-defined events:
--
-- * @'OnAdd' c@ - called after @c@ has been added on an entity in which it wasn't already present.
-- * @'OnSet' c@ - called after @c@ has been inserted on an entity. Runs after @OnAdd@.
-- * @'OnRemove' c@ - called before @c@ is removed from an entity.
--
-- @'OnAddRel'@, @'OnSetRel'@, @'OnRemoveRel'@ are the relationship equivalents of the above.

-- $msg
-- Instace @message@ for a type to make it a message.
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
-- @write@ writes a new message:
--
-- @
-- Messages.'Mischief.ECS.Messages.write' (Foo 5)
-- @
--
-- @read@ drains the buffer of a certain type of message:
--
-- @
-- messages <- Messages.'Mischief.ECS.Messages.read' \@Foo
-- @
--
-- @read@ will get all messages that haven't yet been read by the current system, erasing them from the buffer.

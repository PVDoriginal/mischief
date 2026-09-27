{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Queries Tutorial
-- Description: How To Mischief.
--
-- [Previous Chapter: Events and Messages]("Mischief.ECS.Tutorial.Guides.Events")
--
-- [Next Chapter: (Doc) App and Plugins]("Mischief.ECS.Tutorial.Documentation.App")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Change
  ( -- * Learn You an ECS for Great Mischief! - 2.11. Change Detection
    -- $intro

    -- * Events
    -- $events

    -- * Query Filters
    -- $filters

    -- * Avoiding False Changes
    -- $false

    -- * Component Hooks
    -- $hooks

    -- * [Next Chapter: (Docs) App and Plugins]("Mischief.ECS.Tutorial.Documentation.App")
  )
where

import Mischief.ECS

-- $intro
-- This chapter will show you ways of listening and reacting to changes in the World.

-- $events
-- There are various pre-defined events that can be used to listen to when a component is added, inserted, or removed:
--
-- @
-- onAddName :: 'OnAdd' Name -> 'System' ()
-- onAddName OnAdd {entity} = do
--   Just name <- 'single' ['q'|entity. Name|]
--   'info' ['i'|#{entity} was just added the name #{name}|]
-- @
--
-- For insertion and removal you can use @OnSet@ and @OnRemove@ respectively.
--
-- There are also @OnAddRel@, @OnSetRel@, @OnRemoveRel@ which act the same but can listen to relationship changes.
--
-- For instance:
--
-- @
-- onRemoveChildOf :: 'OnRemoveRel' ChildOf -> 'System' ()
-- onRemoveChildOf OnRemoveRel {entity, target} =
--   'info' ['i'|#{entiy} is no longer the child of #{target}|]
-- @

-- $filters
-- You can use the @Changed@ and @Added@ filters in a query to select only entities that have had a component added, or changed.
--
-- With the following query we can get the name of all entities that have had the @Player@ component added in the last frame:
--
-- @
-- ['q'|Name|]
--   & 'qcheck' ['qf'|Added Player|]
--   & 'query'
-- @
--
-- @Added c@ will catch entities that just had @c@ added to them, while @Changed c@ will catch any insertion.
-- If you wish to query for entities that have had a component changed but it wasn't just added, you can do:
--
-- @
-- ['q'|Name|]
--   & 'qcheck' ['qf'|Changed Player, !Added Player|]
--   & 'query'
-- @

-- $false
-- One essential detail to be aware of here is that listening to insertion (through @OnSet@ or @Changed@) doesn't necessarily mean a component's value has been changed!
--
-- The following @insert@ /will/ trigger change detection:
--
-- @
-- p <- 'spawn' (Health 100)
-- 'insert' (Health 100) p
-- @
--
-- To avoid this, you can derive 'Eq' on your components and use @'insertIfNeq'@, which will only perform insertion if the value of the component is different
-- from the current one.

-- $hooks
-- Hooks are specialized events associated directly to a Component instance.
--
-- @
-- instance 'Component' Foo where
--   onAdd = [hook onAddFoo]
--   onSet = [hook onSetFoo]
--   onRemove = [hook onRemoveFoo]
--
-- onAddFoo :: 'HookContext' -> 'System' ()
-- onAddFoo = ...
--
-- onSetFoo :: 'HookContext' -> 'System' ()
-- onSetFoo = ...
--
-- onRemoveFoo :: 'HookContext' -> 'System' ()
-- onRemoveFoo = ...
-- @
--
-- Similar to change events, @HookContext@ has an @.entity@ field you can use to access the entity the hook was triggered on.
--
-- Relationship support dedicated hooks, named @onAddRel@, @onSetRel@, @onRemoveRel@, following the same rules as the normal hooks.
--
-- This is how we can log a message each time @Likes@ is added between two entities:
--
-- @
-- instance 'Component' Likes where
--   onAddRel = ['hookRel' onLikesAdd]
--
-- onLikesAdd :: 'HookContextRel' -> 'System' ()
-- onLikesAdd HookContextRel {entity, target} = 'info' ['i'|#{entity} now likes #{target}!|]
-- @
--
-- There are a number of predefined hooks that are useful when working with relationships, which can be found in "Mischief.ECS.HooksRel".
-- For instance, @addOther@ can be used to automate adding a complementary relationship on the target of a relationship.
--
-- As a quick example of why this is useful, this is how you'd create a @Before@/@After@ relationship between entities:
--
-- @
-- data Before = Before
-- data After = After
--
-- instance 'Component' Before where
--   onAddRel =  [HooksRel.'Mischief.ECS.HooksRel.addOther' ('const' After)]
--
-- instance 'Component' After where
--   'hooks' = [HooksRel.'Mischief.ECS.HooksRel.addOther' ('const' Before)]
-- @
--
-- Now, when you do:
--
-- @
-- 'insert' ('Rel' Before a) b
-- @
--
-- A @Rel After b@ will be inserted automatically on @a@.
--
-- And vice versa.
--
-- There are other interesting hooks, such as ones for automatically cleaning up a relationship when an entity is despawned. Check out "Mischief.ECS.HooksRel" for details!

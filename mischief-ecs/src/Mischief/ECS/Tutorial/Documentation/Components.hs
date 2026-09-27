{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Components Tutorial
-- Description: Tutorial on using @Components@
--
-- This module contains a more in-depth tutorial on @Mischief Components@.
--
-- [Previous Chapter: (Docs) App and Plugins]("Mischief.ECS.Tutorial.Documentation.App")
--
-- [Next Chapter: (Docs) Queries]("Mischief.ECS.Tutorial.Documentation.Queries")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Documentation.Components
  ( -- * Learn You an ECS for Great Mischief! - 3.2. (Docs) Components

    -- * The Name Component
    -- $name

    -- * Operations
    -- $ops

    -- * Meta Components
    -- $meta

    -- * Resources
    -- $resources

    -- * Required Components
    -- $required

    -- * Hooks
    -- $hooks

    -- * Registering Components
    -- $reg

    -- * [Next Chapter: (Docs) Queries]("Mischief.ECS.Tutorial.Documentation.Queries")
  )
where

import Control.Monad (void)
import Data.Default (Default (def))
import Data.Foldable
import GHC.Generics (Generic)
import GHC.Records (HasField)
import Mischief.ECS

-- $introduction
-- A component is any type which derives the 'Component' typeclass. They can be both carriers of data or marker components used for querying (Tags from Flecs):
--
-- Component that carries data.
--
-- @
-- data Health = Health 'Int' deriving ('Component')
-- @
--
-- Marker components.
--
-- @
-- data Player = Player deriving ('Component')
-- data Enemy = Enemy deriving ('Component')
-- @

-- $name
-- @Name@ is a special component provided by Mischief that is internally added to every spawned entity, based on its @Entity@ index,
-- if none is provided on spawn.
--
-- @
-- newtype Name = Name 'String' deriving ('Component')
-- @

-- $ops
-- Mischief offers various operations for inserting and manipulating data into the World:
--
-- * You can spawn entities as bundles of components:
--
-- @
-- player <- 'spawn' (Name \"Player\", Player)
-- @
--
-- * You can insert components on existing entities:
--
-- @
-- 'insert' (Name \"New Player Name\", Health 100)
-- @
--
-- * You can remove components:
--
-- @
-- 'remove' ('C' \@Health, 'C' \@Player) player
-- @
--
-- * You can despawn entities:
--
-- @
-- 'despawn' player
-- @
--
-- Additionally, @insert@ has a couple of variants:
--
-- * @'insertNew'@ only inserts components that aren't already on the entity.
-- * @'insertIfNeq'@ only insert components if they aren't on the entity of if their value differs from the current one.

-- $meta
-- Use @meta@ to access a component's meta entity.
--
-- @
-- m <- 'meta' \@Name
-- @
--
-- Components stored internally on meta entities (should not be modified directly!):
--
-- * @RequiredBy@/@Requires@ - relationship symbolizing requirements.
-- * @DefaultValue@ - the default value of components that are required by others.
-- * @ComponentType@ - the erased type of a component.
-- * @ComponentAddHooks@ / @ComponentSetHooks@ / ... - the hooks each component has.
-- * @IsExclusiveRelationship@ - marker components for relationships that are exclusive.

-- $resources
-- @insertRes@ inserta a resource into the World.
--
-- @
-- 'insertRes' $ MyRes 5
-- @
--
-- @res@ grabs a resource from the World:
--
-- @
-- 'Just' myRes <- 'res' \@MyRes
-- @

-- $required
-- Each component can @require@ a bundle of other components. Each of those components must instance @Default@.
--
-- @
-- data Player = Player
--
-- instance 'Component' Player where
--   'required' = 'require' \@(Position, Health)
--
-- data Position = Position 'Int' 'Int' deriving ('Component', 'Generic', 'Default')
--
-- data Health = Health 'Int' deriving ('Component')
--
-- instance 'Default' Health where
--   'def' = Health 100
-- @
--
-- Requirements are transitive (if @A requires B@ and @B requires C@, then @A requires C@) and /can/ contain cycles.

-- $reg
-- @register@ can be used to register components in to the World. This is normally done automatically when inserting a
-- component for the first time.
--
-- @
-- 'register' \@(Player, Health, Position)
-- @

-- $hooks
-- @Component hooks@ are events associated directly to a Component instance.
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
-- The @onAdd@ and @ohSet@ hooks will always run before @OnAdd@ and @OnSet@ events on that component.
-- @onRemove@ hooks will always run after @OnRemove@ events.

{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: Organizing your Project]("Mischief.ECS.Tutorial.Guides.Organizing")
--
-- [Next Chapter: Query Basics]("Mischief.ECS.Tutorial.Guides.QueryBasics")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Basics
  ( -- * Learn You an ECS for Great Mischief! - 2.2. Entities and Components Basics
    -- $intro

    -- * What's in a Bundle?
    -- $bundle

    -- * Manipulating Data
    -- $manipulating

    -- * Reading Data
    -- $reading

    -- * Required Components
    -- $required

    -- * Metadata
    -- $metadata

    -- * Registering Components
    -- $reg

    -- * [Next Chapter: Query Basics]("Mischief.ECS.Tutorial.Guides.QueryBasics")
  )
where

import Data.Default (Default)
import Mischief.ECS

-- $intro
-- Data in an ECS is manipulated (stored, mutated, destroyed), by performing operations on entities. These operations usually involve bundles of components.

-- $bundle
-- A bundle is a tuple formed of /"things/" that can be /attached/ (or, inserted) to an Entity.
--
-- A bundle can contain:
--
-- * Component Values: Will attach the component.
--
-- @
-- (Name \"A\", Health 100) bob
-- @
--
-- * Relationships: Will attach a relationship with the given entity, containing the given component.
--
-- @
-- ('Rel' Likes charlie) bob
-- @
--
-- * Resources: Will insert the given resource into the world. Is agnostic to the entity it's inserted on.
--
-- @
-- ('Res' (FooRes \"A\"))
-- @
--
-- * From: Will insert the given component on the given entity. Is agnostic to the entity it's inserted on.
--
-- @
-- ('From' alice (Name \"Alice\"))
-- @

-- $manipulating
-- In order to create new data, you spawn an entity with the given data stored in components (through a bundle):
--
-- @
-- bob <- 'spawn' (Name \"Bob\", Player)
-- @
--
-- You can also attach new data to an entity:
--
-- @
-- 'insert' (Health 100, 'Rel' Likes alice) bob
-- @
--
-- To change data, you re-insert the relevant components to the entity:
--
-- @
-- 'insert' (Name \"Bob's New Name\") bob
-- @
--
-- To erase data, you remove the components from the entity:
--
-- @
-- 'remove' ('C' \@Health, 'R' Likes alice) bob
-- @
--
-- You can also @despawn@ an entity, erasing all its data:
--
-- @
-- 'despawn' bob
-- @

-- $reading
-- Data can be retrieved through queries.
--
-- You can get Bob's name and health like this:
--
-- @
-- 'Just' (name, health) \<- 'single' ['q'|bob. Name, Health|]
-- @
--
-- You can grab each entity's name and health like this:
--
-- @
-- x \<- 'query' ['q'|Name, Health|]
-- @
--
-- More on queries in [the next chapter]("Mischief.ECS.Tutorial.Guides.QueryBasics").

-- $required
-- A component @A@ can require any number of other components, as long as they all instance the 'Default' typeclass.
--
-- @
-- data Player
--
-- instance 'Component' Player where
--   required = 'require' \@Health
--
-- data Health = Health 'Int' deriving ('Component')
--
-- instance 'Default' Health where
--   def = Health 100
-- @
--
-- When inserting @Player@ on an entity, @Health 100@ will be immediately inserted as well, as long as the @Health@ component
-- wasn't already inserted.

-- $metadata
-- Each component has a corresponding entity in the World.
-- The components on that entity store information about the component itself. Such as which archetypes it is part of.
--
-- A component's entity can be accessed by using @meta@.
--
-- Getting the entities of the @Name@ and @Player@ components:
--
-- @
-- x <- 'meta' \@Name
-- y <- 'meta' \@Player
-- @
--
-- Most users should avoid tinkering with Meta Components unless they have a good reason to,
-- and should absolutely never remove or change any components added to them by the @ECS@.

-- $reg
-- @Registering@ a component involves spawning its meta entity and adding the corresponding data.
--
-- Each component is registered automatically the first time it is inserted on an Entity, so you don't usually
-- have to worry about registration.
--
-- Queries are also smart about components; if you query or filter for a component that hasn't been registered yet, they will just
-- assume that component can't be be on any Entity. Queries can't perform registration themselves, because they're not allowed to mutate
-- the world in any way.
--
-- However, there may be /extremely/ niche situations where you want to register components earlier than normal, which is where manual registration comes in:
--
-- @
-- 'register' \@(Player, Health, Position)
-- @
--
-- One such situation could be wanting to check the requirements between multiple components on runtime.
-- If a component hasn't been registered yet, it won't show up when you query for components that require a specific component.
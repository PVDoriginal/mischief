{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Components Tutorial
-- Description: Tutorial on using @Components@
--
-- This module contains a more in-depth tutorial on @Mischief@ @Components@.
-- It is recommended to read "MischiefECS" first.
module MischiefECS.Tutorial.Components
  ( -- * Introduction
    -- $introduction

    -- * Query Results
    -- $results

    -- * Meta Components
    -- $meta

    -- * Required Components
    -- $required
  )
where

import Data.Default (Default (def))
import Data.Foldable
import GHC.Generics (Generic)
import MischiefECS (require)
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Spawn
import MischiefECS.Entities
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Modify
import MischiefECS.World.Query
import MischiefECS.World.Remove
import MischiefECS.World.Spawn

-- $introduction
-- A component is any type which derives the 'Component' typeclass. They can be both carriers of data or marker components used for querying (@Tags@ from @Flecs@):
--
-- @
-- -- Component that carries data.
-- data Health = Health 'Int' deriving ('Component', 'Queryable')
-- -- Marker components.
-- data Player = Player deriving ('Component', 'Queryable')
-- data Enemy = Enemy deriving ('Component', 'Queryable')
-- @
--
-- Components can be @inserted@ and @removed@ from entities:
--
-- @
-- foo <- 'spawn' ('Name' "Foo")
-- bar <- 'spawn' ('Name' "Bar", Enemy)
-- baz <- 'spawn' ('Name' "Baz", Player, Enemy)
--
-- 'insert' (Health 0, Player) foo
--
-- -- Re-inserting a component changes its value.
-- 'insert' ('Name' "New Baz Name") baz
--
-- 'remove' @(Enemy, 'Name') baz
-- @
--
-- They can be @queried@ and @changed@:
--
-- @
-- -- Get the Name and Health of all players that aren't enemies.
-- players <- 'query'' @('Name', 'Health') $ 'with' @Player '&.' 'without' @Enemy
--
-- 'for_' players $ \(name, health) -> do
--  'liftIO' $ 'print' $ "Healing " ++ 'show' name
--  'modify' health $ (\(Health x) -> Health (x + 1))
-- @
--
-- They can be stored as @resources@ (also called @singletons@):
--
-- @
-- data Players = Players ['Entity'] deriving (Component, Queryable)
-- addPlayer :: 'Entity' -> Players -> Players
-- addPlayer entity (Players list) = Players (entity : list)
--
-- setup :: 'System' ()
-- setup = 'insertRes' $ Players []
--
-- handleNewPlayer :: 'OnInserted' Player -> 'System' ()
-- handleNewPlayer event = do
--  'Just' players <- 'res' @Players
--  'modify' players $ addPlayer event.target
-- @
--
-- 'Name' is a special component provided by Mischief that is added to every spawned entity, if it contains none, and
-- is usually equal to @'show' 'Entity'@.

-- $results
-- A @'Result' c@ is a wrapper around the component @c@ that's produced by a query.
--
-- @
-- newtype 'Result' c = 'Result' (c, 'Entity')
-- @
--
-- The inner component's value can be obtained via the @value@ function.
--
-- @
-- 'Just' name <- 'get' @'Name' e
-- let name' = 'value' name
-- @
--
-- @
-- name  :: 'Result' 'Name'
-- name' :: 'Name'
-- @
--
-- If the component has record fields, every field will be inherited by the 'Result' (via a 'HasField' instance)
--
-- @
-- data Pos = Pos {x :: 'Float', y :: 'Float'}
-- @
--
-- @
-- 'Just' pos <- 'get' @Pos e
-- let x = pos.x
-- let y = pos.y
-- @
--
-- @
-- pos :: 'Result' Pos
-- x   :: 'Float'
-- y   :: 'Float'
-- @
--
-- Some typeclasses, namely 'Show', 'Eq', 'Ord' are also implemented for a @'Result' c@ if they are for the underlying @c@.

-- $meta
-- Each component has a corresponding entity in the 'World', called a @Meta Component@.
--
-- The components on that entity store information about the component itself. Such as which archetypes it is part of.
--
-- A component's entity can be accessed by using @'meta' :: forall c. ('Component' c) => 'System' 'Entity'@.
-- For instance, the entity of 'Name' is @'meta' \@'Name'@.
--
--  This is, in fact, how @resources@ are stored: by inserting the value of a component unto its own corresponding entity.
--
-- @
-- 'res' :: forall c. ('Queryable' c, 'Component' c) => 'System' ('Maybe' ('Result' c))
-- 'res' = do
--   meta <- 'meta' @c
--   'get' @c meta
-- @
--
-- @
-- 'insertRes' :: forall r. ('Component' r, 'Bundle' r) => r -> 'System' ()
-- 'insertRes' res = do
--   entity <- 'meta' @r
--   'insert' res entity
-- @
--
-- Most users should avoid tinkering with @Meta Components@ unless they have a good reason to,
-- and should absolutely never remove or change any components added to them by the @ECS@.

-- $required
-- Each component can have a list of components that it @requires@.
--
-- If two components, @B@ and @C@ are required by component @A@, they will be added to each entity that @A@ is added to.
--
-- @Requirements@ can be set via a custom instance of 'Component'.
--
-- @
-- data B = B deriving ('Generic', 'Component', 'Default')
-- data C = C deriving ('Generic', 'Component', 'Default')
--
-- data A = A
-- instance 'Component' A where
--   'required' = 'require' @(B, C)
-- @
--
-- As implied above, in order for a component to be required by another, it needs to implement 'Default'.
-- That can either be a 'Generic' derive, or a fully custom instance:
--
-- @
-- data Health = Health 'Int'
-- instance 'Default' Health where
--   'def' = Health 0
-- @

{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Systems Tutorial
-- Description: Tutorial on using @Systems@
--
-- This module contains a more in-depth look into the @Mischief App and Plugins@.
--
-- It isn't as technically interesting as the other chapters of the tutorial, focusing more on organization and high-level logic.
--
-- [Previous Chapter: Coding a Dungeon Game]("Mischief.ECS.Tutorial.Dungeon")
--
-- [Next Chapter: Components]("Mischief.ECS.Tutorial.Components")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.App
  ( -- * Learn You an ECS for Great Mischief! - 3. App and Plugins
    -- $intro

    -- * Modular Features
    -- $modular_features

    -- * [Next chapter: Components]("Mischief.ECS.Tutorial.Components")
  )
where

import Control.Monad.Reader
import Mischief.ECS

-- $intro
-- The @'App'@ is a thin wrapper around the @'World'@ that works as an interface for plugging in
-- various behavior and features in modular fashion, via @Plugins@.
--
-- A Mischief game / app usually starts by creating an app and adding one or more plugins to it.
--
-- @
-- main :: 'IO' ()
-- main = do
--   app <- 'newApp'
--   'addPlugin' \@MainPlugin app
--   'runApp' app
--
-- data MainPlugin
--
-- instance 'Plugin' MainPlugin where
--   init = 'info' \"Hello!\"
-- @
--
-- A @Plugin@ instance has two optional functions:
--
-- 1. An initialization system that will be ran at the very beginning of the app.
--
-- This system is usually used to schedule other systems, or to initialize data, such as resources and
-- observers.
--
-- @
-- init :: 'System' ()
-- init = do
--   'insertRes' (Health 0)
--   'void' $ 'spawn' ('Observer' onDamage)
--
--   'systems' movePlayer
--     & 'schedule' 'Update'
-- @
--
-- 2. A list of plugins this plugin depends on.
--
-- Any plugin can be made into a dependency using the @dep@ function:
--
-- @
-- deps = ['dep' \@PlayerPlugin, 'dep' \@EnemyPlugin, 'dep' \@PhysicsPlugin]
-- @
--
-- It is guaranteed that the init systems of a plugin's dependencies will run before its own init.

-- $modular_features
-- Mischief is intended to let you cleanly separate and organize your logic.
--
-- For instance, you may have a @PhysicsPlugin@ that adds physics to your game, a @RenderPlugin@ which renders objects, a @LevelPlugin@ that spawns your levels.
--
-- @
-- instance 'Plugin' MainPlugin where
--   deps _ = ['dep' \@PhysicsPlugin, 'dep' \@RenderPlugin, 'dep' \@LevelPlugin]
-- @
--
-- Ideally, each of these plugins would add their own independent features. So, if you were to remove @physicsPlugin@, your entities simply wouldn't
-- move and collide anymore, but the rest of the app would work just fine.
--
-- Note that, internally, @PhsyicsPlugin@ could also be subdivided into its own plugins with separate roles:
--
-- @
-- data PhysicsPlugin
--
-- instance 'Plugin' PhysicsPlugin where
--   deps = ['dep' \@CollisionPlugin, 'dep' \@MovePlugin]
-- @
--
-- This also lets the various packages of @Mischief@ be modular, and makes it easy for third party library developers to create plugins that you just plug into your app with ease!

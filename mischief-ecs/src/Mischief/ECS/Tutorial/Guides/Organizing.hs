{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: Coding a Dungeon Game]("Mischief.ECS.Tutorial.Introdution.Dungeon")
--
-- [Next Chapter: Entities and Components Basics]("Mischief.ECS.Tutorial.Guides.Basics")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Guides.Organizing
  ( -- * Learn You an ECS for Great Mischief! - 2.1. Organizing your Project
    -- $intro

    -- * Modular Plugins and Features
    -- $plugins

    -- * Files
    -- $files

    -- * Be Consistent!
    -- $consistency

    -- * [Next Chapter: Entities and Components Basics]("Mischief.ECS.Tutorial.Guides.Basics")
  )
where

import Mischief.ECS

-- $intro
-- It's a common problem when working with a game framework to wonder where and how to organize your logic.
-- Luckily, Mischief comes with a few tips and guidelines on the subject.

-- $plugins
-- Modularily is highly encouraged in everything you do. You should strive to design your codebase so that each
-- part of it can be easily plugged in and out without affecting the functionality of anything outside of it.
--
-- For instance, you may have a @PhysicsPlugin@ that adds physics to your game, a @RenderPlugin@ which renders objects, a @LevelPlugin@ that spawns your levels.
--
-- @
-- main :: 'IO' ()
-- main = do
--   app <- 'newApp'
--   'addPlugin' \@PhysicsPlugin
--   'addPlugin' \@RenderPlugin
--   'addPlugin' \@LevelPlugin
-- @
--
-- Ideally, each of these plugins add their own independent features. So if you were to remove @physicsPlugin@, your entities simply wouldn't
-- move and collide anymore, but the rest of the app would work just fine. Dependencies between plugins at the same level should be avoided.
-- For instance, @PhysicsPlugin@ should not ever depend on @RenderPlugin@.
--
-- Internally, @PhsyicsPlugin@ could also be subdivided into its own plugins with separate roles:
--
-- @
-- data PhysicsPlugin
--
-- instance 'Plugin' PhysicsPlugin where
--   deps = ['dep' \@CollisionPlugin, 'dep' \@MovePlugin]
-- @
--
-- And these should follow the same principle of independence.
--
-- Of course, complete modulariy may just not be possible or desireable at points, but we consider it to be a great standard to look up to.

-- $files
-- We encourage placing each plugin in a separate module / file. It's also desireable for the module hierarchy to
-- follow the follow the plugin dependencies. If the @Player@ plugin in @MyGame.Player@ depends on the @Health@ plugin, the latter
-- is ideally placed in a @MyGame.Player.Health@ module.
--
-- We also encourage defining components locally in the module that uses them most. If multiple modules at the same level use the
-- same component, consider placing it in a separete @Common@ module.

-- $consistency
-- Following some of these guidelines may be incompatible with your type of project or the way you prefer to organize things. That's fine!
--
-- The most important thing is to try and keep it all consistent throughout your project. Set clear
-- guidelines and rules for yourself and stick to them!
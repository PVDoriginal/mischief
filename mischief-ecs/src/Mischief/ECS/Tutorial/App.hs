{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Systems Tutorial
-- Description: Tutorial on using @Systems@
--
-- This module contains a more in-depth look into the @Mischief App and Plugins@.
--
-- It isn't as technically interesting as the other chapters of the tutorial, focusing more on organization and high-level logic.
--
-- [Next Chapter: Components]("Mischief.ECS.Tutorial.Components")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.App
  ( -- * Learn You an ECS for Great Mischief! - 1. App and Plugins

    -- * Introduction
    -- $intro

    -- * Modular Features
    -- $modular_features

    -- * Initialization
    -- $init

    -- ** Plugin Resolution
    -- $resolution

    -- * [Next chapter: Components]("Mischief.ECS.Tutorial.Components")
  )
where

import Control.Monad.Reader
import Mischief.ECS

-- $intro
-- The @'App'@ is a thin wrapper around the @'World'@ that works as an interface for plugging in
-- various behavior and features in modular fashion, via @Plugins@.
--
-- A Mischief game / app usually starts by creating an app with a main plugin and then running it.
--
-- @
-- main :: 'IO' ()
-- main = do
--   app <- 'newApp' myPlugin
--   'runApp' app
--
-- data MainPlugin = MainPlugin deriving ('Eq')
--
-- instance 'Plugin' MainPlugin where
--   'Msichief.ECS.App.Plugins.init' _ = 'info' \"Hello!\"
-- @
--
-- From that initial plugin you could add other plugins, each adding new features to your game.
--
-- A @Plugin p@ instance has two optional functions:
--
-- 1. An initialization system that will be ran at the very very beginning of the app.
--
-- @
-- 'Mischief.ECS.App.Plugins.init' :: p -> 'System' ()
-- @
--
-- 2. A collection of plugins that will be added along with this plugin.
--
-- @
-- 'plugins' :: p -> 'Plugins'
-- @
--
-- @Plugins@ is backed by a generic typeclass meant to collect elements of a tuple into a shared storage.
--
-- @
-- 'plugins' _ = 'collect' (FooPlugin, BarPlugin, BazPlugin)
-- @
--
-- You don't need to understand what @collect@ does internally, just be aware of it because it will be used a couple more times in this tutorial.

-- $modular_features
-- Mischief is intended to let you cleanly separate and organize your logic.
--
-- For instance, you may have a @PhysicsPlugin@ that adds physics to your game, a @RenderPlugin@ which renders objects, a @LevelPlugin@ that spawns your levels.
--
-- @
-- instance 'Plugin' MainPlugin where
--   'plugins' _ = 'collect' (PhysicsPlugin, RenderPlugin, LevelPlugin)
-- @
--
-- Ideally, each of these plugins would add their own independent features. So, if you were to remove @physicsPlugin@, your entities simply wouldn't
-- move and collide anymore, but the rest of the app would work just fine.
--
-- This also lets the various packages of @Mischief@ to be modular, and makes it easy for third party library developers to create plugins that you just plug into your app with ease!
--
-- Note that, internally, @PhsyicsPlugin@ could also be subdivided into its own plugins with separate roles:
--
-- @
-- data PhysicsPlugin = PhysicsPlugin deriving ('Eq')
--
-- instance 'Plugin' PhysicsPlugin where
--   'plugins' _ = 'collect' (CollisionPlugin, MovePlugin)
-- @

-- $init
-- So what exactly can you do within the @init@ function of a @Plugin@?
--
-- The answer is, pretty much anything.
--
-- As the name suggests, it is typically used for initializing some sort of logic or behavior within your app. This is
-- usually done by scheduling systems, inserting some resource, and spawning important entities, such as observers.
--
-- @
-- 'Mischief.ECS.App.Plugins.init' _ = do
--   [Systems]("Mischief.ECS.Systems").'Mischief.ECS.Systems.add' $ 'Update' foo '`after`' bar
--   [Systems]("Mischief.ECS.Systems").'Mischief.ECS.Systems.add' $ 'Startup' baz
--
--   'insertRes' $ SomeRes 5
-- @
--
-- Note that the initialization systems are guaranteed to run before everything else in the app, /but/ there is no guarantee of order between them.
-- So, a Plugin should /never/ depend on another Plugin being added before it.

-- $resolution
-- Have you noticed that the 'Plugin' typeclass requires the type to also derive 'Eq'? Let's dig into that.
--
-- To give a practical example, think of an @SDLPlugin@ which initializes an @SDL@ program, opening
-- an actual window and allowing for things such as collecting input and rendering stuff.
--
-- @
-- data SDLPlugin = SDLPlugin deriving ('Eq')
--
-- instance 'Plugin' SDLPlugin where
--   'Mischief.ECS.App.Plugins.init' _ = createSDLWindow (Resolution (500, 500))
-- @
--
-- Two separate Plugins exist, an @InputPlugin@ which handles reading input events, and a @RenderPlugin@ which handles processing shaders and rendering.
-- Both those Plugins need @SDLPlugin@ to function properly.
--
-- @
-- data InputPlugin = InputPlugin deriving ('Eq')
--
-- instance 'Plugin' InputPlugin where
--   'plugins' _ = 'collect' SDLPlugin
--
-- data RenderPlugin = RenderPlugin deriving ('Eq')
--
-- instance 'Plugin' RenderPlugin where
--   'plugins' _ = 'collect' SDLPlugin
-- @
--
-- So it's a handy feature to have multiple plugins depend on the same plugin. But what if @SDLPlugin@ looked like this:
--
-- @
-- data SDLPlugin = SDLPlugin {res :: ('Int', 'Int')} deriving ('Eq')
--
-- instance 'Plugin' SDLPlugin where
--   'Mischief.ECS.App.Plugins.init' (SDLPlugin {res}) = createSDLWindow (Resolution res)
-- @
--
-- @SDLPlugin@ now produces different effects based on its value. This means there is a possibility that the following happens:
--
-- @
-- data InputPlugin = InputPlugin deriving ('Eq')
--
-- instance 'Plugin' InputPlugin where
--   'plugins' _ = 'collect' (SDLPlugin (300, 300))
--
-- data RenderPlugin = RenderPlugin deriving ('Eq')
--
-- instance 'Plugin' RenderPlugin where
--   'plugins' _ = 'collect' (SDLPlugin (500, 500))
-- @
--
-- What if you were to add both @InputPlugin@ and @RenderPlugin@ to your app? Which variant of @SDLPlugin@ would it pick?
--
-- This conflict is why the @Plugin@ typeclass requires @Eq@. When the same plugin is added into the app twice,
-- their two values will be compared and, if equal, the plugin will be added, otherwise the app will crash.
--
-- This ensures a level of control over how plugins do their initialization, without requiring that a plugin is only added once.

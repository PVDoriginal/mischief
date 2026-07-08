{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- Module: Systems Tutorial
-- Description: Tutorial on using @Systems@
--
-- This module contains a more in-depth look into the @app and plugins@. It is recommended to read "MischiefECS" first.
--
-- This isn't as technically interesting as the other chapters of the tutorial, focusing more on organization and high-level logic.
module MischiefECS.Tutorial.App
  ( -- * Learn You an ECS for Great Mischief! - 1. App and Plugins

    -- * Introduction
    -- $intro

    -- * Modular Features
    -- $modular_features

    -- * Plugins
    -- $plugins

    -- ** Running systems
    -- $run

    -- ** Scheduling systems
    -- $scheduling_systems

    -- ** Adding observers
    -- $add_observer

    -- ** Adding messages
    -- $add_message

    -- * [Next chapter: Components]("MischiefECS.Tutorial.Components")
  )
where

import Control.Monad.Reader
import MischiefECS

-- $intro
-- The @'App'@ is mainly a wrapper around a 'World' that works as an interface for plugging in
-- various behavior and features in modular fashion, via @'Plugin's@.
--
-- @
-- main :: 'IO' ()
-- main = do
--   app <- 'newApp' myPlugin
--   'runApp' app
--
-- myPlugin :: 'Plugin' ()
-- myPlugin = do
--   -- add something to the app
-- @
--
-- As you can see, each time you create a new @App@ with 'newApp', you need to provide a plugin. This will be the starting block of your game / app.
--
-- From that initial plugin you could add other plugins, each adding new features to your game.

-- $modular_features
-- Mischief is intended to let you cleanly separate and organize your logic.
--
-- For instance, you may have a @physicsPlugin@ that adds physics to your game, a @renderPlugin@ which renders objects, a @levelPlugin@ that spawns your entities.
--
-- @
-- main :: 'IO' ()
-- main = do
--   app <- 'newApp' $ do
--     'addPlugins' [physicsPlugin, renderPlugin, levelPlugin]
-- @
--
-- Ideally, each of these plugins would add their own independent features. So, if you were to remove @physicsPlugin@, your entities simply wouldn't
-- move and collide anymore, but the rest of the app would work just fine.
--
-- This also lets the various packages of @Mischief@ to be modular, and makes it easy for third party library developers to create plugins that you just plug into your app!
--
-- Note that, internally, @phsyicsPlugin@ could also be subdivided into its own plugins with separate roles!
--
-- @
-- phsyicsPlugin :: 'Plugin' ()
-- physicsPlugin = do
--   'addPlugin' collisionPlugin
--   'addPlugin' movePlugin
-- @
--
-- Additionally, similar to @systems@, @plugins@ are a wrapper around @'ReaderT' 'App' 'IO'@, and the composable nature of @Haskell@ monads means
-- even 'addPlugin' itself is a 'Plugin'!

-- $plugins
-- So what exactly can you do within a @plugin@, besides adding other plugins?

-- $run
-- For one, you can just run any system on the spot, mutating the 'World'!
--
-- @
-- playerPlugin :: 'Plugin' ()
-- playerPlugin = do
--   'run' spawnPlayer
--
-- spawnPlayer :: 'System' ()
-- spawnPlayer = 'void' $ 'spawn' (Name \"Player\", Player)
-- @
--
-- It is /extremely/ unadvisable however to write complex logic like this though, since it will run the moment the plugin is added to the app. Meaning that if you have two plugins:
--
-- @
-- data SomeRes = SomeRes 'Int' deriving ('Component', 'Queryable', 'Show')
--
-- plugin1 :: 'Plugin' ()
-- plugin1 = do
--   'run' $ do
--     'insertRes' $ SomeRes 5
--
-- plugin2 :: 'Plugin' ()
-- plugin2 = do
--   'run' $ do
--     r <- 'res' @SomeRes
--     'liftIO' $ 'print' r
-- @
--
-- Here, @plugin2@ uses a resource inserted by @plugin1@, meaning that you need to ensure @plugin1@ is added first, which should be avoided.
--
-- However, @plugin1@ from is completely fine, since it just initializes a resource and doesn't actually depend on anything. This is what a lot of plugins
-- are used for!
--
-- If you're familiar with @Bevy@, @'run' $ 'insertRes' ... @ is essentially the equivalent of @app.insert_resource(...)@.

-- $scheduling_systems
-- Plugins are also used to schedule systems into the app.
--
-- For instance, in the previous example, the system from @plugin2@ can just be scheduled to run on @Startup@. The schedules only run when you call 'runApp', after the plugins have already been
-- added.
--
-- @
-- plugin2 :: 'Plugin' ()
-- plugin2 = do
--   'addSystems' 'Startup' $ do
--     r <- 'res' @SomeRes
--     'liftIO' $ 'print' r
-- @
--
-- @'addSystems'@ takes a 'Schedule' and a 'SystemConfig', which can either be a normal @'System' ()@, a tuple of systems, or something like '@a `'after'` b'@, where both @a@ is 'SystemConfig' and
-- @b@ is a system.
--
-- @
-- plugin :: 'Plugin' ()
-- plugin = do
--   addSystems Update $ (a, d) `after` b `before` c
--   addSystems Update (b, c)
-- @
--
-- More details on scheduling systems in @Chapter 3@.

-- $add_observer
-- Observers are a very powerful primitive.
--
-- @
-- plugin :: 'Plugin' ()
-- plugin = do
--   addObserver obs1
--   addObserver obs2
--
-- obs1 :: 'OnInsert' 'Name' -> 'System' ()
-- obs1 = ...
--
-- obs2 :: 'OnRemove' Player -> 'System' ()
-- obs2 = ...
-- @
--
-- They are functions that look like @E -> 'System' ()@, where @E@ is any type that derives 'Event'.
-- More on observers in @Chapter TODO@.

-- $add_message
-- Messages are a good way of communicating between systems. They also need to be initialized via plugins.
--
-- @
-- data SomeMessage = SomeMessage 'String' deriving ('Message')
--
-- plugin :: 'Plugin' ()
-- plugin = do
--   'addMessage' @SomeMessage
-- @
--
-- More on messages in @Chapter TODO@.

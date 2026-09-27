{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: Common Patterns]("Mischief.ECS.Tutorial.Guides.Change")
--
-- [Next Chapter: (Docs) Components]("Mischief.ECS.Tutorial.Documentation.Components")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Documentation.App
  ( -- * Learn You an ECS for Great Mischief! - 3.1. (Docs) App and Plugins

    -- * The App
    -- $app

    -- * Plugins
    -- $plugins

    -- * [Next Chapter: (Docs) Components]("Mischief.ECS.Tutorial.Documentation.Components")
  )
where

import Control.Monad.Reader
import Mischief.ECS

-- $app
-- Functions for creating and interacting with the App:
--
--
-- @newApp@ creates a new App in IO.
--
-- @
-- app \<- 'newApp'
-- @
--
-- @addPlugin@ adds a plugin to the app.
--
-- @
-- 'addPlugin' app \@MyPlugin
-- @
--
-- @runApp@ starts the default schedule loop of the app:
--
-- @
-- 'runApp' app
-- @
--
-- Additionally, you can use @runSystem@ to run custom systems:
--
-- @
-- 'runSystem' ('info' \"Hello\") app.world
-- @

-- $plugins
-- Each plugin has two optional functions, @init@ and @deps@:
--
-- @
-- data MyPlugin
--
-- instance 'Plugin' MyPlugin where
--   init = 'info' \"Hello\"
--   deps = ['dep' \@PlayerPlugin, 'dep' \@EnemyPlugin]
-- @
--
-- @dep@ turns any Plugin into a dependency.
--
-- When a plugin is added via @addPlugin@, it will add and run the inits of all its dependenices, if they weren't already added, and
-- then run its own init.
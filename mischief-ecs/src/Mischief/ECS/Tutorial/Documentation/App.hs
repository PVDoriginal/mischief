{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |
-- [Previous Chapter: Common Patterns]("Mischief.ECS.Tutorial.Guides.Change")
--
-- [Next Chapter: (Docs) Components]("Mischief.ECS.Tutorial.Documentation.Components")
--
-- [Main Page]("Mischief.ECS")
module Mischief.ECS.Tutorial.Documentation.App
  ( -- * Learn You an ECS for Great Mischief! - 3.1. (Doc) App and Plugins
    -- $intro

    -- * The App
    -- $app

    -- * Plugins
    -- $plugins

    -- * [Next Chapter: (Docs) Components]("Mischief.ECS.Tutorial.Documentation.Components")
  )
where

import Control.Monad.Reader
import Mischief.ECS

-- $intro
-- The @'App'@ is a thin wrapper around the @'World'@ that works as an interface for plugging in
-- various behavior and features in modular fashion, via @Plugins@.

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
-- @init@ is a system ran after all the dependencies' (@deps@) inits have been ran.

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
--
-- Module: ECS
-- Description: The ECS standing at the core of Mischief.
--
-- This library contains the ECS used by the Mischief Game Engine.
module Mischief.ECS
  ( -- * LLM Disclaimer
    -- $ai

    -- * What is Mischief?
    -- $mischief

    -- * Learn You an ECS for Great Mischief!
    -- $tutorials
    module Mischief.ECS.App,
    module Mischief.ECS.App.Plugins,
    module Mischief.ECS.App.Schedules,
    module Mischief.ECS.App.SystemConfig,
    module Mischief.ECS.App.Systems,
    module Mischief.ECS.Archetypes,
    module Mischief.ECS.Archetypes.Graph,
    module Mischief.ECS.Collectable,
    module Mischief.ECS.Components,
    module Mischief.ECS.Components.Bundle,
    module Mischief.ECS.Components.BundleTypes,
    module Mischief.ECS.Components.Common,
    module Mischief.ECS.Components.Required,
    module Mischief.ECS.Components.Runnable,
    module Mischief.ECS.Components.Spawn,
    module Mischief.ECS.Entities,
    module Mischief.ECS.Events,
    module Mischief.ECS.Hooks,
    module Mischief.ECS.Log,
    module Mischief.ECS.Mappable,
    module Mischief.ECS.Messages,
    module Mischief.ECS.Prelude,
    module Mischief.ECS.Relationships,
    module Mischief.ECS.Relationships.ChildOf,
    module Mischief.ECS.Relationships.Graph,
    module Mischief.ECS.Relationships.Order,
    module Mischief.ECS.Relationships.Tree,
    module Mischief.ECS.Tables,
    module Mischief.ECS.Time,
    module Mischief.ECS.Utils,
    module Mischief.ECS.World,
    module Mischief.ECS.World.Change,
    module Mischief.ECS.World.Defer,
    module Mischief.ECS.World.Insert,
    module Mischief.ECS.World.Modify,
    module Mischief.ECS.World.Par,
    module Mischief.ECS.World.Prefs,
    module Mischief.ECS.Resources,
    module Mischief.ECS.World.Query,
    module Mischief.ECS.World.Query.Queryable,
    module Mischief.ECS.World.Query.QueryFilter,
    module Mischief.ECS.World.Query.Markers,
    module Mischief.ECS.EventDef,
    module Mischief.ECS.World.Query.TH,
    module Mischief.ECS.World.Remove,
    module Mischief.ECS.World.Spawn,
    module Mischief.ECS.World.Utils,
    TestTest (..),
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Mischief.ECS.App
import Mischief.ECS.App.Plugins
import Mischief.ECS.App.Schedules
import Mischief.ECS.App.SystemConfig
import Mischief.ECS.App.Systems
import Mischief.ECS.Archetypes
import Mischief.ECS.Archetypes.Graph
import Mischief.ECS.Collectable (Collectable, EraseIntoStorage (..), collect)
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.BundleTypes
import Mischief.ECS.Components.Common
import Mischief.ECS.Components.Required
import Mischief.ECS.Components.Runnable
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.EventDef
import Mischief.ECS.Events
import Mischief.ECS.Graph
import Mischief.ECS.Hooks
import Mischief.ECS.Log
import Mischief.ECS.Mappable
import Mischief.ECS.Messages (Message)
import Mischief.ECS.Prelude
import Mischief.ECS.Relationships
import Mischief.ECS.Relationships.ChildOf
import Mischief.ECS.Relationships.Graph
import Mischief.ECS.Relationships.Order
import Mischief.ECS.Relationships.Tree
import Mischief.ECS.Resources
import Mischief.ECS.Systems
import Mischief.ECS.Tables
import Mischief.ECS.Time
import Mischief.ECS.Utils
import Mischief.ECS.Vec
import Mischief.ECS.World
import Mischief.ECS.World.Change
import Mischief.ECS.World.Defer
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Modify
import Mischief.ECS.World.Par
import Mischief.ECS.World.Prefs
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.Markers
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Query.TH
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Systems
import Mischief.ECS.World.Utils

data TestTest = TestTest

-- x = quoteD

-- x = $quoteE

qq :: System ()
qq = do
  -- x <- $(quoteC ''ChildOf)
  undefined

-- $pre
-- This version of Mischief isn't meant for public use. It's just a pre-release published mostly to showcase the documentation.

-- $ai
-- No LLM / AI was used in making Mischief. Now and forever.

-- $mischief
-- Mischief has various meanings. It can be a group of rats. Or it can refer to being naughty and playful.
--
-- This Mischief, however, is an open-source @ECS Game Engine@! In other words...
--
-- @
-- Mischief :: Entity -> Component c => System ()
-- @
--
-- Mischief takes great inspiration from [@Bevy@](https://bevy.org/) and [@Flecs@](https://www.flecs.dev/flecs/), and
-- was written in @100% Haskell@, taking advantage of its great ergonomics and strong type system.
-- It is a refreshing spin on the functional and data-driven paradigms.
--
-- == Package
--
-- This package contains only @mischief-ecs@, the ECS at the core of Mischief. Other packages, such as @mischief-input@, @mischief-assets@,
-- @mischief-render@ are planned, but haven't yet been developed to the same level as the ECS itself.
--
-- == Design Goals
--
-- These are the main design goals of Mischief:
--
-- * __Ergonomic and Accessible__. Mischief's main purpose is to provide a very clean, intuitive, inherently opinionated, high-level API. This is the main way we differentiate ourselves from other ECS's.
-- Learning to use Mischief should be a painless and intuitive process.
-- * __Functional and Data Driven__. We strive to find a balance between functional programming and data-driven design. These are two concepts that don't
-- interact very often, but that both stand at the core of Msichief.
-- * __Modular__. Mischief is meant to be modular, allowing you to plug packages in and out, whether they are made by us or a third party.
--
-- == Highlights
--
-- Here are some highlights of Mischief:
--
-- * __Highly Dynamic__ . Want to disable a system? Despawn it!
-- * __Fancy Queries__. Complete with transitive queries and filters.
-- * __Quasi-Queries__. Scripting language for writing queries.
-- * __Rich Systems__. Monadic Systems? Yes please.
-- * __Book__. Mischief has its very own book! See below.
-- * __Relationships__. Bob Likes Charlie.
-- * __Events__.
-- * And much much more..
--
--
-- == Performance
--
-- Performance-wise, Mischief still has a long way to go; there are many easy performance gains that we have just been too busy to implement, as we've
-- been mostly focusing on ergonomics, modularity, and ease-of-use.
-- This is a pretty strong @Archetype ECS@ however, and it is possible to bring it to about the same /asymptotic/ performance as Bevy or Flecs, although
-- there will probably always be a layer of indirection (or 20) that makes it a bit slower, due to the high-level, boxed, nature of Haskell.

-- $tutorials
-- In order to learn how to use Mischief, you can read through the official book:
--
-- (1) [Startup Guide]("Mischief.ECS.Tutorial.Startup")
-- (2) [Coding a Dungeon Game]("Mischief.ECS.Tutorial.Dungeon")
-- (3) [App and Plugins]("Mischief.ECS.Tutorial.App")
-- (4) [Components]("Mischief.ECS.Tutorial.Components")
-- (5) [Relationships]("Mischief.ECS.Tutorial.Relationships")
-- (6) [Queries]("Mischief.ECS.Tutorial.Queries")
-- (7) [Systems]("Mischief.ECS.Tutorial.Systems")
-- (8) [Events and Messages]("Mischief.ECS.Tutorial.Events")

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
--
-- Module: ECS
-- Description: The ECS standing at the core of Mischief.
--
-- This library contains the ECS used by the Mischief Game Engine.
--
-- This module has an overview of the different features and tools provided by the ECS.
-- More information on each subject can be found in the Tutorial modules, as well as spread
-- throughout the other various modules.
module Mischief.ECS
  ( -- * What is Mischief?
    -- $mischief

    -- * Learn You an ECS for Great Mischief! - Introduction
    -- $learn

    -- ** Components
    -- $components

    -- ** Entities
    -- $entities

    -- ** Systems
    -- $systems

    -- ** Scheduling
    -- $scheduling

    -- ** Plugins
    -- $plugins

    -- ** Logging
    -- $logging

    -- ** Queries
    -- $queries

    -- ** Filters
    -- $filters

    -- ** Change Detection
    -- $change

    -- ** Resources
    -- $resources

    -- ** Events
    -- $events

    -- ** Special Events
    -- $special_events

    -- * In-Depth Tutorials
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
    module Mischief.ECS.Components.Hooks,
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
    module Mischief.ECS.World.Query,
    module Mischief.ECS.World.Query.Queryable,
    module Mischief.ECS.World.Query.QueryFilter,
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
import Mischief.ECS.App.Scheduler
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
import Mischief.ECS.Components.Hooks
import Mischief.ECS.Components.Required
import Mischief.ECS.Components.Runnable
import Mischief.ECS.Components.Spawn
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Graph
import Mischief.ECS.Hooks
import Mischief.ECS.Log
import Mischief.ECS.Mappable
import Mischief.ECS.Messages
import Mischief.ECS.Prelude
import Mischief.ECS.Relationships
import Mischief.ECS.Relationships.ChildOf
import Mischief.ECS.Relationships.Graph
import Mischief.ECS.Relationships.Order
import Mischief.ECS.Relationships.Tree
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
import Mischief.ECS.World.Query.Quasi
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable
import Mischief.ECS.World.Query.Val
import Mischief.ECS.World.Remove
import Mischief.ECS.World.Spawn
import Mischief.ECS.World.Systems
import Mischief.ECS.World.Utils

data TestTest = TestTest

e :: Entity
e = Entity 0 0

-- x = quoteD

-- x = $quoteE

qq :: System ()
qq = do
  -- x <- $(quoteC ''ChildOf)
  undefined

-- $mischief
-- ings. It can be a group of rats. Or it can refer to being naughty and playful.
--
--  This Mischief, however, is an @ECS Game Engine@! In other words...
--
--  @
--  Mischief :: Entity -> Component c
--  Mischief has a few different mean => System ()
--  @
--
--  Mischief takes great inspiration from [@Bevy@](https://bevy.org/) and [@Flecs@](https://www.flecs.dev/flecs/), and
--  was written in @100% Haskell@, taking advantage of its @great ergonomics and strong type system@.
--  It is a refreshing spin on the @functional@ and @data-driven@ paradigms.
--
--  This package contains only @mischief-ecs@, the ECS at the core of Mischief. Other packages, such as @mischief-input@, @mischief-assets@,
--  @mischief-render@ are planned, but haven't yet been developed to the same level as the ECS itself.
--
--  Performance-wise, Mischief still has a long way to go; there are many easy performance gains that we have just been too busy to implement, as we've
--  been mostly focusing on @ergonomics, modularity, and ease-of-use@.
--  This is a pretty strong @Archetype ECS@ however, and it is possible to bring it to about the same /asymptotic/ performance as @Bevy@ or @Flecs@, although
--  there will probably always be a layer of indirection (or 20) that makes it a bit slower, due to the high-level, boxed, nature of Haskell.

-- $learn
-- This module will go over some brief notions to give you an idea of how Mischief works, setting you up for reading the other tutorials that go more in-depth.

-- $components
-- A @Component@ is a piece of data attached to an entity. This can be any type that derives the Component typeclass, for instance:
--
-- @
-- data Health = Health 'Int' deriving ('Component')
-- @
--
-- Additionally, a tuple of components is generally referred to as a 'Bundle'.

-- $entities
-- An @Entity@ is a group of components that exist within the World.
--
-- The actual @'Entity'@ type is just an index pointing towards those components.

-- $systems
-- @Systems@ are the lifeblood of Mischief. A system is a function which mutates the 'World', the global space containing all data stored by the ECS.
--
-- Mischief's systems are quite different from other ECS's. They are fully composable, taking full advantage of Haskell's monadic actions:
--
-- @
-- foo :: 'System' ()
-- foo = do
--   bar
--   baz
--
-- bar :: 'System' ()
-- bar = ...
--
-- baz :: 'System' ()
-- baz = ...
-- @
--
-- There are many helpful predefined systems, such as @spawn@, used for creating a new entity from a bundle of components:
--
-- @
-- foo <- 'spawn' ('Name' \"Foo\")
-- @
--
-- You can insert new components on an entity with @insert@. This can also be used to change the value of existing components:
--
-- @
-- 'insert' (Name \"Healthy Foo\", Health 100) foo
-- @
--
-- Components can be removed using @remove@ (more on what @C@ is later):
--
-- @
-- 'remove' ('C' @Health) foo
-- @
--
-- And @despawn@ deletes an entity, along with all its components, from the world.
--
-- @
-- 'despawn' foo
-- @
--
-- Every system also has 'IO' access via @liftIO@.
--
-- @
-- helloWorld :: 'System' ()
-- helloWorld = 'liftIO' $ 'print' "Hello World!"
-- @

-- $scheduling
-- A @Schedule@ is a group of systems that can be ran using 'runSchedule'.
--
-- Systems can be added to a schedule using @[Systems]("Mischief.ECS.Systems").'Mischief.ECS.Systems.add'@, which is actually just a convenient wrapper around
-- some spawn and insert operations.
--
-- @
-- import "Mischief.ECS.Systems" qualified as [Systems]("Mischief.ECS.Systems")
--
-- data SomeSchedule = SomeSchedule deriving ('Schedule')
--
-- scheduleFoo :: 'System' ()
-- scheduleFoo = do
--   [Systems]("Mischief.ECS.Systems").'Mischief.ECS.Systems.add' SomeSchedule foo
--
-- foo :: 'System' ()
-- foo = ...
-- @
--
-- There are a few predefined schedules which are ran automatically by the app, such as 'Startup', which runs once at the start of the app, and
-- 'Update', which runs once per frame.
--
-- @Systems.add@ also allows explicitly ordering systems via '`after`' and '`before`'.
--
-- @
-- [Systems]("Mischief.ECS.Systems").'Mischief.ECS.Systems.add' SomeSchedule $ foo '`after`' bar '`before`' baz
-- @

-- $plugins
-- A @Plugin@ is a configuration added to the @App@. Each plugin runs an initialization system and can have multiple other plugins as dependencies. They are generally
-- used for giving resources initial values, and for scheduling systems.
--
-- @
-- main :: 'IO' ()
-- main = do
--  app <- 'newApp' HelloPlugin
--  'runApp' app
--
-- data HelloPlugin = HelloPlugin deriving ('Eq')
--
-- instance 'Plugin' HelloPlugin where
--
--   'Mischief.ECS.App.Plugins.init' :: 'System' ()
--   'Mischief.ECS.App.Plugins.init' = 'info' \"Hello World!\"
--
--   'plugins' :: 'Plugins'
--   'plugins' = 'collect' (somePlugin, someOtherPlugin)
--
-- @
--
-- As shown above, an @App@ needs a @Plugin@ when it is created. That will be the source of all logic that goes into the app.

-- $logging
--
-- Systems can invoke custom log actions via "Mischief.ECS.Log", backed by @Co-log@.
--
-- @
-- foo :: 'System' ()
-- foo = do
--   'info'  "Hello!"
--   'warn'  "Be warned.."
--   'err'   "Critical error!"
--   'panic' "Stopping execution."
--
-- @
--
-- Note that these actions expect a  @'Text'@ rather than a @'String'@. You can use 'text' to convert a 'Show'-able data to Text, and '<>' to concatenate two Texts.
--
-- You also need the @OverloadedStrings@ language extension to implicitly convert a string literaly to @Text@.
--
-- @
-- let name = \"Player's Name\"
-- 'info' $ "The player's name is " <> 'text' name
-- @

-- $queries
-- @Queries@ are operations that allow reading data from entities. For instance, we may use the following query to get
-- the @Name@ and @Health@ of each entity in our World:
--
-- @
-- x <- 'query' ('C' \@Name, 'C' \@Health)
-- @
--
-- The @'C'@ type used above is a marker that indicates the type of data you are querying for. @C@ is short for @Component@. There are other similar markers, such
-- as @'R'@, short for @Relationship@.
--
-- Let's check the type of the above query:
--
-- @
-- x :: [('Result' Name, 'Result' Health)]
-- @
--
-- @'Result'@ is a wrapper type around the component's value that carries additional information. The inner value can be obtained using the @'value'@ function.
-- A @Result@ can be used for operations such as 'set', 'modify', 'delete'. For instance, we can do this to increase the Health of each entity and then print
-- it:
--
-- @
-- 'for_' x $\(name, health) -> do
--   'modify' health $ (\(Health x) -> Health (x + 1))
--   'info' $ 'text' name '<>' \" now has health: \" '<>' 'text' health
-- @
--
-- You can query for components of a given @'Entity'@ using @'get'@:
--
-- @
-- 'Just' name <- 'get' ('C' \@'Name') player
-- @
--
-- Keep in mind that doing @'Just' name <-@ will only work if the value was not 'Nothing'. This is essentially the equivalent of an @.unwrap()@ from @Rust@.
-- Only do this is if you are absolutely certain the query will be valid, otherwise it is recommended to treat both cases:
--
-- @
-- name <- 'get' ('C' \@'Name') player
--
-- case name of
--   'Nothing' -> 'return' ()
--   'Just' name -> do
--     ...
-- @
--
-- Querying for @('C' \@Name, 'C' \@Health)@ will only select entities which contain those components. You can use @'M'@ (short for @Maybe@) to
-- also include entities that don't necessarily have a specific component:
--
-- @
-- x <- 'query' ('C' \@Name, 'M' \@Health)
-- @
--
-- @
-- x :: [('Result' Name, 'Maybe' ('Result' Health))]
-- @

-- $filters
-- @Filters@ can be added to a query to filter which entities it can return.
--
-- For instance, the @With@ and @Without@ filters can be used to include or exclude one or more components from a query.
--
-- The following query will return the @Name@ of all players that have @Health@ but aren't
-- @Enemies@.
--
-- @
-- 'query'' ('C' \@'Name') ('With' \@(Player, Health), 'Without' \@Enemy)
-- @
--
-- The @query'@ above is a variant of @query@ that also takes a filter as an argument.
-- Other types of queries follow the same pattern.
--
-- The @'|.'@ operator can be used to express @or@ between filters, and @'Not'@ can be used to negate a filter.

-- $change
-- @Change detection@ is implemented through query filters.
--
-- The @Added@ and @Changed@ filters can check if certain components have been added or changed since the current system last ran.
--
-- The following query returns all Entities that have either just been added the @Player@ component, or that have had their @Health@ changed:
--
-- @
-- 'query'' ('C' \@'Entity') ('Added' \@Player '|.' 'Changed' \@Health)
-- @

-- $resources
-- @Resources@ are special singleton-type values. Any component can be a resource.
--
-- @
-- data FooRes = FooRes Int deriving ('Component')
-- @
--
-- A resource can be inserted using @insertRes@:
--
-- @
-- 'insertRes' $ FooRes 5
-- @
--
-- The value of a resource can be obtained using @res@:
--
-- @
-- 'Just' foo <- 'res' \@FooRes
-- @

-- $events
-- @Events@ and @Observers@ are important primitives of Mischief.
--
-- An event is just a data type deriving the @Event@ typeclass:
--
-- @
-- data MyEvent = MyEvent 'Int' deriving ('Event')
-- @
--
-- Any event can be triggered using the @trigger@ system:
--
-- @
-- 'trigger' $ MyEvent 10
-- @
--
-- Triggering an event will cause any observer listening to that event to run.
--
-- An observer is just a function that looks like @e -> 'System' ()@, where @e@ is an Event. For instance:
--
-- @
-- onMyEvent :: MyEvent -> 'System' ()
-- onMyEvent (MyEvent x) = 'info' $ \"MyEvent was triggered with value \" '<>' 'text' x
-- @
--
-- In order to activate an observer, it just needs to be spawned into the world:
--
-- @
-- obs <- 'spawn' $ 'Observer' onMyEvent
-- @

-- $special_events
-- There are some built-in events which are automatically triggered by the ECS for you.
--
-- @OnInsert c@ triggers each time the @c@ component is inserted (or re-inserted) on an entity. It also contains the entity itself.
--
-- @
-- handleNewPlayer :: 'OnInsert' Player -> 'System' ()
-- handleNewPlayer event = 'info' $ "The player component was added to " <> 'text' event.entity
-- @
--
-- @'OnRemove' c@ is another such event, which is triggered /before/ the @c@ component is removed from an entity.

-- $tutorials
-- Now that you (hopefully) have a basic understanding of how things work, you can start reading the rest of the @Tutorial@ modules.
--
-- (1) [App and Plugins]("Mischief.ECS.Tutorial.App")
-- (2) [Components]("Mischief.ECS.Tutorial.Components")
-- (3) [Systems]("Mischief.ECS.Tutorial.Systems")

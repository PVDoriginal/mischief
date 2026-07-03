{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- Module: ECS
-- Description: The ECS standing at the core of Mischief.
--
-- This library contains the ECS used by the Mischief Game Engine.
module MischiefECS
  ( -- * Entity -> Component c => System ()

    -- ** Components
    -- $components

    -- ** Entities
    -- $entities

    -- ** Systems
    -- $systems

    -- ** Plugins
    -- $plugins

    -- ** Scheduling
    -- $scheduling

    -- ** Ordering
    -- $ordering

    -- * Other chapters
    module MischiefECS.Components,
    module MischiefECS.Entities,
    module Systems,
    module MischiefECS.Tables,
    module MischiefECS.World,
    module MischiefECS.Components.Bundle,
    module MischiefECS.Components.Required,
    module MischiefECS.World.Query,
    module MischiefECS.World.Insert,
    module MischiefECS.World.Par,
    module MischiefECS.World.Spawn,
    module MischiefECS.World.Modify,
    module MischiefECS.World.Remove,
    module MischiefECS.World.Defer,
    module MischiefECS.App,
    module MischiefECS.Utils,
    module MischiefECS.App.Schedules,
    module MischiefECS.App.SystemConfig,
    module MischiefECS.Messages,
    module MischiefECS.SDL,
    module MischiefECS.Events,
    module MischiefECS.Time,
    module MischiefECS.Relationships,
    module MischiefECS.Relationships.ChildOf,
    ParSystem,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Language.Haskell.TH
import MischiefECS.App
import MischiefECS.App.Schedules
import MischiefECS.App.SystemConfig
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Required
import MischiefECS.Entities
import MischiefECS.Events
import MischiefECS.Messages
import MischiefECS.Relationships
import MischiefECS.Relationships.ChildOf
import MischiefECS.SDL
import MischiefECS.Tables
import MischiefECS.Time
import MischiefECS.Tutorial.Systems as Systems
import MischiefECS.Utils
import MischiefECS.World
import MischiefECS.World.Defer
import MischiefECS.World.Insert
import MischiefECS.World.Internal (ParSystem)
import MischiefECS.World.Modify
import MischiefECS.World.Par
import MischiefECS.World.Query
import MischiefECS.World.Remove
import MischiefECS.World.Spawn
import MischiefECS.World.Utils

-- $components
-- A 'Component' is a piece of data attached to an entity. This can be any type that derives the 'Component' typeclass, for instance:
--
-- @
-- data Health = Health 'Int' deriving ('Component')
-- @
--
-- Additionally, a tuple of 'Component's is generally referred to as a 'Bundle'.

-- $entities
-- An 'Entity' is just an index towards a specific group of 'Component's.

-- $systems
-- 'System's are the lifeblood of Mischief. A 'System' is a 'Monad' which operates on the 'World': the global space containing all data of the 'App'.
--
-- For instance, you can use @'spawn' :: ('Bundle' b) => b -> 'System' 'Entity'@ to spawn a new 'Bundle' into the 'World' and get back its 'Entity'.
--
-- @
-- data Player = Player deriving ('Component')
--
-- spawnPlayer :: 'System' ()
-- spawnPlayer = do
--  e <- 'spawn' ('Name' "Player Name", Player)
--  'return' ()
-- @
--
-- Another useful 'System' is @'despawn' :: 'Entity' -> 'System' ()@ which receives an 'Entity' and despawns it from the 'World', deleting all the 'Component's.
--
-- Every 'System' also has 'IO' access via 'liftIO'.

-- $plugins
-- A 'Plugin' is a 'Monad' that alters the 'App' itself by changing various configurations. A 'Plugin' can also be used to run 'System's on the spot.
--
-- @
-- main :: 'IO' ()
-- main = do
--  app <- 'newApp' [plugin]
--  'runApp' app
--
-- plugin :: 'Plugin' ()
-- plugin = do
--  -- The player will be spawned while the app is being initialized,
--  -- before any schedules.
--  'run' spawnPlayer
-- @
--
-- As shown above, an 'App' expects a list of 'Plugin's when it is created.
-- New 'Plugin's can also be added via @'addPlugin' :: 'Plugin' () -> 'Plugin' ()@.

-- $scheduling
-- Any @'System' ()@ can be added to a 'Schedule', programming it to run at a certain time.
--
-- There are many predefined 'Schedule's, such as 'Startup', which runs once at the start of the 'App', and
-- 'Update', which runs once per frame.
--
-- Creating custom 'Schedule's and running them on-demand is also possible (TODO: actually add this).
--
-- For instance, the following plugin will spawn a player in the 'Startup' schedule:
--
-- @
-- 'addSystems' Startup spawnPlayer
-- @

-- $ordering
-- The scheduler also allows custom ordering between 'System's.
--
-- Let's consider the following 'System':
--
-- @
-- changePlayerName :: 'System' ()
-- changePlayerName = do
--  Just name <- 'single'' @'Name' $ 'with' @Player
--  'set' name $ 'Name' "New Player Name"
-- @
--
-- Don't worry if you're confused by the 'single'', we'll get to that later. All you need to know
-- is that this system changes the name of the player.
--
-- However, if we just add them both to the 'Startup' schedule, there's no guarantee
-- that this sytem will run after the one spawning the player. So instead, we can do is specify that we want this 'System'
-- to run after the other. This can be done when adding it through a 'Plugin':
--
-- @
-- 'addSystems' 'Startup' spawnPlayer
-- 'addSystems' 'Startup' $ changePlayerName `'after'` spawnPlayer
-- @

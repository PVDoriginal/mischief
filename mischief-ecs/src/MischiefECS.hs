{-# LANGUAGE AllowAmbiguousTypes #-}

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

    -- * Tutorials
    -- $tutorials
    module MischiefECS.Tutorial.Systems,
    module MischiefECS.Components,
    module MischiefECS.Entities,
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
import Data.Foldable (for_)
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
import MischiefECS.Tutorial.Systems
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
-- Additionally, a tuple of components is generally referred to as a 'Bundle'.

-- $entities
-- An 'Entity' is just an index towards a specific group of components.

-- $systems
-- @Systems@ are the lifeblood of @Mischief@. A 'System' is a 'Monad' which operates on the 'World': the global space containing all data of the 'App'.
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
-- Another useful 'System' is @'despawn' :: 'Entity' -> 'System' ()@ which receives an 'Entity' and despawns it from the 'World', deleting all its components.
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
-- As shown above, an 'App' expects a list of Plugins when it is created.
-- New Plugins can also be added via @'addPlugin' :: 'Plugin' () -> 'Plugin' ()@.

-- $scheduling
-- Any @'System' ()@ can be added to a 'Schedule', telling it to run at a certain time.
--
-- There are many predefined schedules, such as 'Startup', which runs once at the start of the app, and
-- 'Update', which runs once per frame.
--
-- Creating custom schedules and running them on-demand is also possible (TODO: actually add this).
--
-- For instance, the following plugin will spawn a player in the 'Startup' schedule:
--
-- @
-- 'addSystems' Startup spawnPlayer
-- @

-- $ordering
-- The scheduler also allows custom ordering between systems.
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
-- that this sytem will run after the one spawning the player. So instead, we can do is specify that we want /this/ system
-- to run after the other. This can be done when adding it through a 'Plugin':
--
-- @
-- 'addSystems' 'Startup' spawnPlayer
-- 'addSystems' 'Startup' $ changePlayerName '`after`' spawnPlayer
-- @

-- $queries
-- @Queries@ are operations that allow reading specific data from the ECS. For instance, we may use the following query to get
-- the 'Name' and @Health@ (defined above) of each entity in our 'World':
--
-- @
-- x <- 'query' @('Name', Health)
-- @
--
-- Note that the 'Queryable' typeclass needs to be derived on each 'Component' that you wish to query. So, @Health@ would now look like this:
--
-- @
-- data Health = Health 'Int' deriving ('Component', 'Queryable')
-- @
--
-- Notice how we use the '@' type hints to specify what components we are querying for. The result will be a list of tuples of those components,
-- an element corresponding to each entity that has those specific components. This type hint is usually referred to as a 'QueryData'.
--
-- In this case, the type of @x@ will be @x :: [('Result' Name, 'Result' Health)]@.
--
-- 'Result' is a wrapper type around the 'Component' that carries additional information, such as the 'Entity' that 'Component' belongs to.
--
-- This allows for special systems such as 'set', 'modify', 'delete' to operate directly on a 'Result'. For instance, we can do this:
--
-- @
-- 'for_' x $\(name, health) -> do
--  'liftIO' $ 'print' $ 'show' name ++ " has " ++ 'show' health
--  'set' name $ 'Name' "New Name"
--  'modify' health $ (\(Health x) -> Health (x + 1))
-- @
--
-- A component's value can be obtained at any time from a 'Result' by using @'value' :: 'Result' c -> c@.
--
-- 'single' is type of query that attempts to return the components of a single entity, if only one such entity exists.
-- If a query over the same components would return @[a]@, 'single' will return @'Maybe' a@
--
-- 'get' is another special type of query that grabs the specified components of a provided entity.
-- If we have a @player :: 'Entity'@, we can use the following to obtain it's 'Name'.
--
-- @
-- 'Just' name <- 'get' @'Name' player
-- @
--
-- Keep in mind that doing @'Just' x <-@ will only work if the value was not 'Nothing'. This is essentially the equivalent of an @.unwrap()@ from Rust.
-- Only do this is if you are absolutely certain the query will be valid, otherwise it is recommended to treat both cases, such as:
--
-- @
-- name <- 'get' @'Name' player
--
-- case name of
--  'Nothing' -> 'return' ()
--  'Just' name -> do
--     'liftIO' $ 'print' name
-- @
--
-- There are special types of 'QueryData' that don't return a 'Result'. For instance, querying @/@'Entity'@ will just return an 'Entity' in that place in the tuple.

-- $filters
-- @Filters@ are special modifiers to a query that can filter out certain entities based on various properties.
--
-- For instance, the 'with' and 'without' 'QueryFilter's can be used to include or exclude one or more components from a 'QueryData'.
--
-- Multiple @filters@ can be combined using the '&.' and '|.' operators.
--
-- For instance, the following 'query'' will return the 'Name' of all players (@Entities@ with the @Player@ 'Component') that have @Health@ but aren't
-- @Enemies@ (don't have the @Enemy@ 'Component').
--
-- @
-- players <- 'query'' \@'Name' $ 'with' \@(Player, Health) '&.' 'without' \@Enemy
-- @
--
-- Notice how 'query'' is a variant of 'query' that also takes a 'QueryFilter' as an argument.
-- This is also true for other queries, such as 'single''.

-- $change
-- 'QueryFilter's can also look at changes that have happened for a 'Component'
--
-- The 'added' filter can check if mentioned 'Component's were added since the 'System' was last ran.
--
-- @
-- -- Query entities that have just receives the Player component.
-- x <- 'query'' @'Entity' $ 'added' @Player
-- @
--
-- Similarly, 'changed' can check if 'Component's have changed their values.

-- $resources
-- @Resources@ are special singleton-type entities. Essentially, they can be used to store /global/ 'Component's that can be easily
-- grabbed and changed at any time.
--
-- @'insertRes' :: ('Component' c) => c -> 'System' ()@ can be used to add a new @Resource@ into the 'World', or to update its value if it already exists.
--
-- @'res' :: forall c. ('Component' c) => 'System' ('Maybe' c)@ can be used to retrieve the value of a resource (it usually required a type hint).

-- $events
-- 'Event's are types that can trigger 'Observer's.
--
-- For instance, having this 'Event':
--
-- @
-- data Foo = Foo 'Int' deriving ('Event', 'Show')
-- @
--
-- We can create the following 'Observer':
--
-- @
-- onFoo :: Foo -> 'System' ()
-- onFoo foo = do
--  'liftIO' $ 'print' foo
-- @
--
-- As you can see, an 'Observer' is any function @E -> 'System' ()@, where @E@ is a type deriving 'Event'.
--
-- 'Observer's can be added to an 'App' via the 'addObserver' 'Plugin':
--
-- @
-- 'addObserver' onFoo
-- @
--
-- And you can use @'trigger' :: ('Event' e) => e -> 'System' ()@ to cause them to run by triggering their specific 'Event's:
--
-- @
-- trigger (Foo 5)
-- @

-- $special_events
-- There are some special 'Event's already defined by the ECS.
--
-- @'OnInsert' c@ triggers each time the @c@ 'Component' is inserted on an 'Entity', and carries the actual 'Entity' via @.target@.
--
-- @
-- handleNewPlayer :: 'OnInsert' Player -> 'System' ()
-- handleNewPlayer event = do
--  'liftIO' $ 'print' $ "The player component was added to " ++ 'show' event.target
-- @
--
-- @'OnRemove' c@ is another such 'Event', which is triggered before the @c@ 'Component' is removed from an 'Entity'.

-- $tutorials
-- Now that you have a basic understanding of how It is recommended to read the rest of the @Tutorial@ modules.
--
-- (1) "MischiefECS.Tutorial.Components"
-- (2) "MischiefECS.Tutorial.Systems"
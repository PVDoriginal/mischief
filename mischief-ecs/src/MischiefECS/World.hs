{-# LANGUAGE AllowAmbiguousTypes #-}

-- {-# OPTIONS_GHC -Wno-redundant-constraints #-}
module MischiefECS.World where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.IORef
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable
import MischiefECS.Archetypes
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Internal
import MischiefECS.Entities
import MischiefECS.Events.Internal
import MischiefECS.Tables
import MischiefECS.World.Prefs

-- | The World is the main data structure storing the entities, components, archetypes, and so on.
data World = World
  { archetypes :: Archetypes,
    components :: Components,
    entities :: Entities,
    tables :: Tables,
    events :: IORef [ErasedEvent],
    -- | A list of deferred systems that are ran at the end of each system, or can be flushed manually using 'flush'.
    deferred :: IORef [System ()],
    deferredAsync :: TVar [System ()],
    -- | The current Tick, incremented each time a system is ran, used for change detection.
    tick :: IORef Tick,
    -- | ID of the current system.
    systemId :: SystemId,
    -- | The current frame.
    frame :: IORef Frame,
    prefs :: WorldPrefs
  }

newtype Frame = Frame Int deriving (Show, Eq, Ord)

newtype SystemId = SystemId {entity :: Entity} deriving (Show, Eq, Ord)

newWorld :: IO World
newWorld = do
  archetypes <- emptyArchetypes
  components <- emptyComponents
  entities <- emptyEntities
  tables <- emptyTables
  deferred <- newIORef []
  deferredAsync <- newTVarIO []
  events <- newIORef []
  tick <- newIORef (Tick 0)
  frame <- newIORef (Frame 0)
  let prefs = newPrefs

  return
    World
      { archetypes,
        components,
        entities,
        tables,
        events,
        deferred,
        deferredAsync,
        tick,
        systemId = SystemId (Entity 0 0),
        frame,
        prefs
      }

setSystemId :: SystemId -> World -> World
setSystemId systemId World {archetypes, components, entities, tables, deferred, deferredAsync, tick, frame, events, prefs} =
  World {archetypes, components, entities, tables, deferred, deferredAsync, tick, systemId, frame, events, prefs}

setDeferred :: IORef [System ()] -> World -> World
setDeferred deferred World {archetypes, components, entities, tables, deferredAsync, tick, frame, events, systemId, prefs} =
  World {archetypes, components, deferred, entities, tables, deferredAsync, tick, frame, events, systemId, prefs}

setPrefs :: WorldPrefs -> World -> World
setPrefs prefs World {archetypes, components, entities, tables, deferred, deferredAsync, tick, frame, events, systemId} =
  World {archetypes, components, entities, tables, deferred, deferredAsync, tick, frame, events, systemId, prefs}

-- | Increment the World's tick.
tick :: System ()
tick = do
  world <- ask
  liftIO $ modifyIORef' world.tick (\(Tick x) -> Tick $ x + 1)

-- | A System is a set of instructions applied over a World.
-- It can be added to the app to be ran on a certain schedule.
--
-- A system is actually a 'ReaderT' 'World' 'IO', meaning you can 'ask' for the World,
-- or do IO operations directly by using 'liftIO'.
-- type System = ReaderT World IO
newtype System a = System (ReaderT World IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader World, MonadFail)

runSystem :: System a -> World -> IO a
runSystem (System !r) = runReaderT r

instance PrimMonad System where
  type PrimState System = PrimState IO
  primitive = System . lift . primitive

-- | A 'Component' that's inserted automatically on each 'Entity', but can also be set manually.
newtype Name = Name String
  deriving anyclass (Component)
  deriving newtype (Eq)

instance Show Name where
  show :: Name -> String
  show (Name name) = show name

forkPrefs :: (WorldPrefs -> WorldPrefs) -> System () -> System ()
forkPrefs f s = do
  world <- ask
  let world' = setPrefs (f world.prefs) world
  liftIO $ runSystem s world'

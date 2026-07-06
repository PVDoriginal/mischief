module MischiefECS.World
  ( World (..),
    newWorld,
    tick,
    setSystemId,
    setDeferred,
    setPrefs,
    forkPrefs,
    System (..),
    runSystem,
    Name (..),
    Frame (..),
    SystemId (..),
  )
where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.IORef (IORef, modifyIORef', newIORef)
import MischiefECS.Archetypes (Archetypes, emptyArchetypes)
import MischiefECS.Components
  ( Component,
    Components,
    Tick (..),
    emptyComponents,
  )
import MischiefECS.Entities
  ( Entities,
    Entity (Entity),
    emptyEntities,
  )
import MischiefECS.Events.Internal (ErasedEvent)
import MischiefECS.Tables (Tables, emptyTables)
import MischiefECS.World.Prefs (WorldPrefs, newPrefs)

-- | @The World@ is the main data structure storing the entities, components, archetypes, and everything else that lives in our app.
data World = World
  { -- | Archetype storage.
    archetypes :: Archetypes,
    -- | Components storage.
    components :: Components,
    -- | Entities storage.
    entities :: Entities,
    -- | Archetype tables. Storage for the actual data of components.
    tables :: Tables,
    -- | List of events to be ran at the next sync point.
    events :: IORef [ErasedEvent],
    -- | List of deferred system to be ran at the next sync point. They can also be flushed manually using 'flush'.
    deferred :: IORef [System ()],
    -- | Secondary list of deferred systems stored in a TVar, meant to be used when running systems asynchronously.
    -- They are ran at the first available sync point, and can also be flushed manually with 'flushAsync'.
    deferredAsync :: TVar [System ()],
    -- | The current tick, incremented each time a system is ran, used for change detection.
    tick :: IORef Tick,
    -- | Id of the current system.
    systemId :: SystemId,
    -- | The current frame.
    frame :: IORef Frame,
    -- | Certain toggleable settings.
    prefs :: WorldPrefs
  }

newtype Frame = Frame Int deriving (Show, Eq, Ord)

newtype SystemId = SystemId {entity :: Entity} deriving (Show, Eq, Ord)

-- | Create a new World in IO.
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

-- | Change the current SystemId of the World.
setSystemId :: SystemId -> World -> World
setSystemId systemId World {archetypes, components, entities, tables, deferred, deferredAsync, tick, frame, events, prefs} =
  World {archetypes, components, entities, tables, deferred, deferredAsync, tick, systemId, frame, events, prefs}

-- | Set the list of deferred systems of the World.
setDeferred :: IORef [System ()] -> World -> World
setDeferred deferred World {archetypes, components, entities, tables, deferredAsync, tick, frame, events, systemId, prefs} =
  World {archetypes, components, deferred, entities, tables, deferredAsync, tick, frame, events, systemId, prefs}

-- | Set new WorldPrefs for the World.
setPrefs :: WorldPrefs -> World -> World
setPrefs prefs World {archetypes, components, entities, tables, deferred, deferredAsync, tick, frame, events, systemId} =
  World {archetypes, components, entities, tables, deferred, deferredAsync, tick, frame, events, systemId, prefs}

-- | Increment the World's Tick.
tick :: System ()
tick = do
  world <- ask
  liftIO $ modifyIORef' world.tick (\(Tick x) -> Tick $ x + 1)

-- | A System is a set of instructions applied over a World.
-- It can be added to the App to be ran on a certain Schedule.
--
-- A system is actually a wrapper around @'ReaderT' 'World' 'IO'@, meaning you can 'ask' for the World,
-- or do IO operations by using 'liftIO'.
newtype System a = System (ReaderT World IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader World, MonadFail)

-- | Run a 'System' with the given 'World' inside 'IO'
runSystem :: System a -> World -> IO a
runSystem (System !r) = runReaderT r

instance PrimMonad System where
  type PrimState System = PrimState IO
  primitive = System . lift . primitive

-- | Special component inserted automatically on most entities.
--
-- Can be inserted manually to give each entity a custom name.
newtype Name = Name String
  deriving anyclass (Component)
  deriving newtype (Eq)

instance Show Name where
  show :: Name -> String
  show (Name name) = show name

-- | Run a 'System' with changed 'WorldPrefs'.
forkPrefs :: (WorldPrefs -> WorldPrefs) -> System a -> System a
forkPrefs f s = do
  world <- ask
  let world' = setPrefs (f world.prefs) world
  liftIO $ runSystem s world'

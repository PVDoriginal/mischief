module Mischief.ECS.World
  ( -- * World
    World (..),
    newWorld,
    setSystemId,
    setDeferred,
    setPrefs,
    forkPrefs,
    Frame (..),
    SystemTools (..),
    worldGet,
    worldGetRAny,
    worldSet,
    worldSpawnByInsert,

    -- * Systems
    System (..),
    SystemId (..),
    runSystem,
    MonadSystem,
    unsafeGetWorld,

    -- * Parallel
    ParSystem (..),
    ParWorld (..),
  )
where

import Colog qualified
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Data.Data
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Mischief.ECS.Archetypes (Archetypes, emptyArchetypes)
import Mischief.ECS.Collectable
import Mischief.ECS.Components
  ( Component,
    Components,
    Rel,
    Tick (Tick),
    emptyComponents,
  )
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Entities
  ( Entities,
    Entity (Entity),
    emptyEntities,
  )
import Mischief.ECS.EventDef
import Mischief.ECS.Hidden
import Mischief.ECS.Tables (Tables, emptyTables)
import Mischief.ECS.World.Prefs (WorldPrefs, newPrefs)

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
    prefs :: WorldPrefs,
    logger :: Colog.LogAction IO Colog.Message,
    tools :: SystemTools
  }

newtype Frame = Frame Int deriving (Show, Eq, Ord)

-- | Unique id assigned to each system that's added to a schedule. Just a wrapper around Entity.
newtype SystemId = SystemId {id :: Entity} deriving (Show, Eq, Ord)

-- | Create a new World in IO.
newWorld :: SystemTools -> IO World
newWorld tools = do
  archetypes <- emptyArchetypes
  components <- emptyComponents
  entities <- emptyEntities
  tables <- emptyTables
  deferred <- newIORef []
  deferredAsync <- newTVarIO []
  events <- newIORef []
  tick <- newIORef (Tick (0, 0))
  frame <- newIORef (Frame 0)
  let prefs = newPrefs

  let logger = Colog.cmap Colog.fmtMessage Colog.logTextStdout

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
        prefs,
        logger,
        tools
      }

-- | Change the current SystemId of the World.
setSystemId :: SystemId -> World -> World
setSystemId systemId world = world {systemId}

-- | Set the list of deferred systems of the World.
setDeferred :: IORef [System ()] -> World -> World
setDeferred deferred world = world {deferred}

-- | Set new WorldPrefs for the World.
setPrefs :: WorldPrefs -> World -> World
setPrefs prefs world = world {prefs}

-- | A System is a set of instructions applied over a World.
-- It can be added to the App to be ran on a certain Schedule.
--
-- A system is actually a wrapper around @'ReaderT' 'World' 'IO'@, meaning you can 'ask' for the World,
-- or do IO operations by using 'liftIO'.
newtype System a = System (ReaderT (Hidden World) IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Hidden World), MonadFail)

-- | Run a 'System' with the given 'World' inside 'IO'
runSystem :: System a -> World -> IO a
runSystem (System !r) w = runReaderT r (hide w)

instance PrimMonad System where
  type PrimState System = PrimState IO
  primitive = System . lift . primitive

-- | Run a 'System' with changed 'WorldPrefs'.
forkPrefs :: (WorldPrefs -> WorldPrefs) -> System a -> System a
forkPrefs f s = do
  world <- unsafeGetWorld
  let world' = setPrefs (f world.prefs) world
  liftIO $ runSystem s world'

-- | Special wrapper around World given to 'ParSystem's.
data ParWorld = ParWorld
  { -- | @Hidden@ ensures users cannot access and mutate the World.
    world :: Hidden World,
    -- | Deferred systems will be collected in this dedicated list and then
    -- merged back into the main deferred list once the parallel systems are joined.
    parDeferred :: IORef [System ()]
  }

-- | A variant of 'System' that contains a 'ParWorld' instead of 'World'.
--
-- @Parallel systems@ will only be able to run systems that are either specifically intended for them or
-- are made to work with any @'MonadSystem'@ (such as queries).
--
-- To run a normal @System ()@, you need to 'Mischief.ECS.World.Defer.defer'!
newtype ParSystem a = ParSystem (ReaderT ParWorld IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader ParWorld, MonadFail, PrimMonad)

class GetWorld a where
  getWorld :: a -> Hidden World

instance (GetWorld (Hidden World)) where
  getWorld = id

instance (GetWorld ParWorld) where
  getWorld x = x.world

-- | Typeclass that can be used to generalize systems to both @'System'@ and @'ParSystem'@.
--
-- Used mainly by queries and other operations which don't mutate the World.
class (GetWorld w, MonadReader w a, Applicative a, MonadFail a, Functor a, Monad a, MonadIO a, PrimMonad a, PrimState a ~ RealWorld) => MonadSystem w a

instance MonadSystem (Hidden World) System

instance MonadSystem ParWorld ParSystem

unsafeGetWorld :: (MonadSystem w m) => m World
unsafeGetWorld = do
  asks (unhide . getWorld)

instance EraseIntoStorage (System ()) [System ()] where
  erase :: System () -> [System ()]
  erase x = [x]

data SystemTools = SystemTools
  { get :: forall c m w. (Component c, MonadSystem w m) => Proxy c -> Entity -> m (Maybe c),
    getRAny :: forall c m w. (Component c, MonadSystem w m) => Proxy c -> Entity -> m (Maybe [Rel c]),
    set :: forall c. (Bundle c) => c -> Entity -> System (),
    spawnByInsert :: forall b. (Bundle b) => Entity -> b -> System ()
  }

worldGet :: forall c m w. (Component c, MonadSystem w m) => Proxy c -> Entity -> m (Maybe c)
worldGet p e = do
  World {tools = SystemTools {get}} <- unsafeGetWorld
  get p e

worldSet :: forall c. (Bundle c) => c -> Entity -> System ()
worldSet c e = do
  World {tools = SystemTools {set}} <- unsafeGetWorld
  set c e

worldGetRAny :: forall c m w. (Component c, MonadSystem w m) => Proxy c -> Entity -> m (Maybe [Rel c])
worldGetRAny p e = do
  World {tools = SystemTools {getRAny}} <- unsafeGetWorld
  getRAny p e

worldSpawnByInsert :: forall b. (Bundle b) => Entity -> b -> System ()
worldSpawnByInsert e b = do
  World {tools = SystemTools {spawnByInsert}} <- unsafeGetWorld
  spawnByInsert e b

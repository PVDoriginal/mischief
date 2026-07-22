{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.ECS.App where

-- import Data.Map
-- import Data.Map qualified as Map

import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Data
import Data.Default
import Data.Foldable
import Data.IORef
import Mischief.ECS.App.Plugins
import Mischief.ECS.App.Schedules
import Mischief.ECS.App.SystemConfig hiding (Before)
import Mischief.ECS.App.SystemDef
import Mischief.ECS.App.Systems (ScheduledIn (ScheduledIn), SystemFunction (SystemFunction), Systems, systemEntity)
import Mischief.ECS.App.Systems qualified as Systems
import Mischief.ECS.Components
import Mischief.ECS.Components.Bundle
import Mischief.ECS.Components.Runnable (Runnable, runFor)
import Mischief.ECS.Components.Spawn (getOrAddComponentId, meta)
import Mischief.ECS.Entities
import Mischief.ECS.Events
import Mischief.ECS.Hidden
import Mischief.ECS.Log
import Mischief.ECS.Relationships.Order
import Mischief.ECS.Systems qualified as Systems
import Mischief.ECS.Tables
import Mischief.ECS.World
import Mischief.ECS.World.Defer
import Mischief.ECS.World.Insert
import Mischief.ECS.World.Query
import Mischief.ECS.World.Query.QueryFilter
import Mischief.ECS.World.Query.Queryable

data App = App
  { world :: World,
    systems :: Systems
  }

newApp :: (Plugin p) => p -> IO App
newApp plugin = do
  world <- newWorld getTools
  systems <- Systems.newSystems

  let app = App {world, systems}

  runSystem appInit app.world
  runSystem (Systems.add Init $ runPluginRec plugin) app.world

  return app

runApp :: App -> IO ()
runApp app = flip runSystem app.world $ do
  startups <- orderEntities =<< query' E (With (C @StartupSchedule))
  err $ text startups
  updates <- orderEntities =<< query' E (With (C @UpdateSchedule))

  liftIO $ runSchedules startups
  liftIO $ runSchedulesLoop updates
  where
    runSchedulesLoop schedules = do
      forever $ do
        runSchedules schedules
        modifyIORef' app.world.frame (\(Frame x) -> Frame $ x + 1)

    runSchedules schedules =
      for_ schedules $ \schedule -> do
        runSystem (runSchedule' schedule) app.world

runSchedule :: (Schedule sch) => sch -> System ()
runSchedule sch = scheduleEntity sch >>= runSchedule'

runSchedule' :: Entity -> System ()
runSchedule' schedule = do
  world <- unsafeGetWorld
  systems <- orderEntities =<< query' E (With (R @ScheduledIn schedule))

  for_ systems $ \systemId -> do
    Just (systemFunction, lastSystemTick) <- get (C @SystemFunction, C @SystemTick) systemId
    currentSystemTick <- liftIO $ readIORef world.tick

    set lastSystemTick (SystemTick currentSystemTick)
    insert (LastSystemTick lastSystemTick.inner) systemId

    Control.Monad.Reader.local (hide . setSystemId (SystemId systemId) . unhide) $ do
      systemFunction.inner
      flush
      flushAsync
      flushEvents
      tick

appInit :: System ()
appInit = do
  insertRes $ def @Schedules

  systems <- liftIO Systems.newSystems
  insertRes systems

  init <- scheduleEntity Init
  pre <- scheduleEntity PreStartup
  startup <- scheduleEntity Startup
  post <- scheduleEntity PostStartup

  for_ [init, pre, startup, post] $ insert StartupSchedule

  insert (Rel Before pre) init
  insert (Rel Before startup) pre
  insert (Rel Before post) startup

  pre <- scheduleEntity PreUpdate
  update <- scheduleEntity Update
  post <- scheduleEntity PostUpdate

  for_ [pre, update, post] $ insert UpdateSchedule

  insert (Rel Before update) pre
  insert (Rel Before post) update

register :: forall c. (Runnable c) => System ()
register = runFor @c registerComponent

registerComponent :: forall c. (Component c) => Proxy c -> System ()
registerComponent c = do
  _ <- getOrAddComponentId (ComponentType c)
  return ()

-- | Insert a resource into this world. If the resource already exists, its value will be overwritten.
insertRes :: forall r. (Component r, Bundle r) => r -> System ()
insertRes res = do
  entity <- meta @r
  insert res entity

getTools :: SystemTools
getTools =
  SystemTools
    { get = toolsGet,
      getRAny = toolsGetRAny,
      set = toolsSet
    }

toolsGet :: forall c m w. (Component c, MonadSystem w m) => Proxy c -> Entity -> m (Maybe c)
toolsGet _ = get (Val (C @c))

toolsSet :: forall c. (Bundle c) => c -> Entity -> System ()
toolsSet = insert

toolsGetRAny :: forall c m w. (Component c, MonadSystem w m) => Proxy c -> Entity -> m (Maybe [Rel c])
toolsGetRAny _ = get (Val (R @c Any))
{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.App where

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
import MischiefECS.App.Plugins
import MischiefECS.App.Schedules
import MischiefECS.App.SystemConfig hiding (Before)
import MischiefECS.App.Systems (LastSystemTick (LastSystemTick), ScheduledIn (ScheduledIn), SystemFunction (SystemFunction), SystemTick (SystemTick), Systems, systemEntity)
import MischiefECS.App.Systems qualified as Systems
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Common
import MischiefECS.Components.Runnable (Runnable, runFor)
import MischiefECS.Components.Spawn (getOrAddComponentId)
import MischiefECS.Entities
import MischiefECS.Events
import MischiefECS.Hidden
import MischiefECS.Log
import MischiefECS.Relationships.Order
import MischiefECS.Tables
import MischiefECS.Utils
import MischiefECS.World
import MischiefECS.World.Defer
import MischiefECS.World.Insert
import MischiefECS.World.Query
import MischiefECS.World.Spawn

data App = App
  { world :: World,
    systems :: Systems
  }

newApp :: (Plugin p) => p -> IO App
newApp plugin = do
  world <- newWorld
  systems <- Systems.newSystems

  let app = App {world, systems}

  runSystem appInit app.world
  runSystem (addSystems Init $ runPluginRec plugin) app.world

  return app

runApp :: App -> IO ()
runApp app = flip runSystem app.world $ do
  startups <- orderEntities =<< (query' @Entity $ with @StartupSchedule)
  err $ text startups
  updates <- orderEntities =<< (query' @Entity $ with @UpdateSchedule)

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
  systems <- orderEntities =<< (query' @Entity $ withRel @ScheduledIn schedule)

  for_ systems $ \systemId -> do
    Just (systemFunction, lastSystemTick) <- get @(SystemFunction, SystemTick) systemId
    currentSystemTick <- liftIO $ readIORef world.tick

    set lastSystemTick (SystemTick currentSystemTick)
    insert (LastSystemTick lastSystemTick.inner) systemId

    Control.Monad.Reader.local (hide . setSystemId (SystemId systemId) . unhide) $ do
      systemFunction.inner
      flush
      flushAsync
      flushEvents
      tick

addSystems :: (Schedule sc, SystemConfig s) => sc -> s -> System ()
addSystems schedule system = do
  let SystemConfigData {systems, edges} = systemConfigData system

  for_ systems $ \system -> systemEntity schedule system

  for_ edges $ \(s1, s2) -> do
    id1 <- systemEntity schedule s1
    id2 <- systemEntity schedule s2
    insert (Rel (Before, id2)) id1

appInit :: System ()
appInit = do
  register @Entity
  insertRes $ def @Schedules

  systems <- liftIO Systems.newSystems
  insertRes systems

  init <- scheduleEntity Init
  pre <- scheduleEntity PreStartup
  startup <- scheduleEntity Startup
  post <- scheduleEntity PostStartup

  for_ [init, pre, startup, post] $ insert StartupSchedule

  insert (Rel (Before, pre)) init
  insert (Rel (Before, startup)) pre
  insert (Rel (Before, post)) startup

  pre <- scheduleEntity PreUpdate
  update <- scheduleEntity Update
  post <- scheduleEntity PostUpdate

  for_ [pre, update, post] $ insert UpdateSchedule

  insert (Rel (Before, update)) pre
  insert (Rel (Before, post)) update

register :: forall c. (Runnable c) => System ()
register = runFor @c registerComponent

registerComponent :: forall c. (Component c) => Proxy c -> System ()
registerComponent c = do
  _ <- getOrAddComponentId (ComponentType c)
  return ()

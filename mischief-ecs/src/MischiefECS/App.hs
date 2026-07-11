{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefECS.App where

-- import Data.Map
-- import Data.Map qualified as Map

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Data
import Data.Default
import Data.Foldable
import Data.IORef
import MischiefECS.App.Scheduler (ScheduleType (..), Scheduler)
import MischiefECS.App.Scheduler qualified as Scheduler
import MischiefECS.App.Schedules
import MischiefECS.App.SystemConfig
import MischiefECS.App.Systems (LastSystemTick (LastSystemTick), SystemFunction, SystemTick (SystemTick), Systems)
import MischiefECS.App.Systems qualified as Systems
import MischiefECS.Components
import MischiefECS.Components.Bundle
import MischiefECS.Components.Runnable (Runnable, runFor)
import MischiefECS.Components.Spawn (getOrAddComponentId)
import MischiefECS.Events
import MischiefECS.Hidden
import MischiefECS.Tables
import MischiefECS.World
import MischiefECS.World.Defer
import MischiefECS.World.Insert
import MischiefECS.World.Query
import MischiefECS.World.Spawn

data App = App
  { world :: World,
    systems :: Systems
    -- scheduler :: Scheduler
  }

newtype Plugin a = Plugin (ReaderT App IO a)
  deriving newtype (Functor, Applicative, Monad, MonadReader App, MonadIO)

runPlugin :: Plugin a -> App -> IO a
runPlugin (Plugin r) = runReaderT r

newApp :: Plugin () -> IO App
newApp plugin = do
  world <- newWorld
  systems <- Systems.newSystems

  let app = App {world, systems}

  runPlugin appInit app
  runPlugin plugin app

  return app

runApp :: App -> IO ()
runApp app = do
  scheduler <-
    runSystem
      ( do
          Just sch <- res @Scheduler
          return $ value sch
      )
      app.world

  startups <- Scheduler.getSchedules StartupSchedule scheduler
  updates <- Scheduler.getSchedules UpdateSchedule scheduler

  runSchedules startups
  runSchedulesLoop updates
  where
    runSchedulesLoop schedules = do
      forever $ do
        runSchedules schedules
        modifyIORef' app.world.frame (\(Frame x) -> Frame $ x + 1)

    runSchedules schedules =
      for_ schedules $ \schedule -> do
        runSystem (runSchedule schedule) app.world

runSchedule :: ScheduleLabel -> System ()
runSchedule schedule = do
  world <- unsafeGetWorld
  Just scheduler <- res @Scheduler
  systems <- liftIO $ Scheduler.getScheduleSystems schedule (value scheduler)

  for_ (concat systems) $ \systemId -> do
    Just (systemFunction, lastSystemTick) <- get @(SystemFunction, SystemTick) systemId.entity
    currentSystemTick <- liftIO $ readIORef world.tick

    set lastSystemTick (SystemTick currentSystemTick)
    insert (LastSystemTick lastSystemTick.inner) systemId.entity

    Control.Monad.Reader.local (hide . setSystemId systemId . unhide) $ do
      systemFunction.inner
      flush
      flushAsync
      flushEvents
      tick

addSystems :: (Schedule sc, SystemConfig s) => sc -> s -> Plugin ()
addSystems schedule system = do
  app <- ask
  let label = ScheduleLabel $ typeOf schedule
  let SystemConfigData {systems, edges} = systemConfigData system

  scheduler <- run $ do
    Just sch <- res @Scheduler
    return $ value sch

  for_ systems $ \system -> do
    systemId <- run $ Systems.getSystemId label system app.systems
    liftIO $ Scheduler.addSystem label systemId scheduler

  for_ edges $ \(s1, s2) -> do
    id1 <- run $ Systems.getSystemId label s1 app.systems
    id2 <- run $ Systems.getSystemId label s2 app.systems
    liftIO $ Scheduler.addSystemEdge label (id1, id2) scheduler

addSchedule :: (Schedule s) => s -> ScheduleType -> Plugin ()
addSchedule schedule scheduleType = do
  scheduler <- run $ do
    Just sch <- res @Scheduler
    return $ value sch

  liftIO $ Scheduler.addSchedule (ScheduleLabel $ typeOf schedule) scheduleType scheduler

addScheduleEdge :: (Schedule s1, Schedule s2) => (s1, s2) -> ScheduleType -> Plugin ()
addScheduleEdge (s1, s2) scheduleType = do
  scheduler <- run $ do
    Just sch <- res @Scheduler
    return $ value sch

  liftIO $ Scheduler.addScheduleEdge (ScheduleLabel $ typeOf s1, ScheduleLabel $ typeOf s2) scheduleType scheduler

addRes :: (Component r, Bundle r) => r -> Plugin ()
addRes r = do
  app <- ask
  liftIO $ runSystem (insertRes r) app.world

initRes :: forall r. (Component r, Bundle r, Default r) => Plugin ()
initRes = addRes $ def @r

addObserver :: (Event e) => (e -> System ()) -> Plugin ()
addObserver observer = do
  app <- ask
  liftIO $ runSystem (spawnObserver (Observer observer) Nothing) app.world

addObserverOrdered :: (Event e) => (e -> System ()) -> Int -> Plugin ()
addObserverOrdered observer order = do
  app <- ask
  liftIO $ runSystem (spawnObserver (Observer observer) $ Just (ObserverOrder order)) app.world

addPlugin :: Plugin () -> Plugin ()
addPlugin plugin = do
  app <- ask
  liftIO $ runPlugin plugin app

addPlugins :: (Foldable t) => t (Plugin ()) -> Plugin ()
addPlugins plugins = for_ plugins addPlugin

run :: System a -> Plugin a
run s = do
  app <- ask
  liftIO $ runSystem s app.world

data Schedules = Schedules {startup :: IORef [TypeRep], update :: IORef [TypeRep]}

newSchedules :: IO Schedules
newSchedules = do
  startup <- newIORef [typeOf Startup]
  update <- newIORef [typeOf PreUpdate, typeOf Update, typeOf PostUpdate]
  return Schedules {startup, update}

appInit :: Plugin ()
appInit = do
  scheduler <- liftIO Scheduler.newScheduler
  run $ insertRes scheduler

  addSchedule Startup StartupSchedule

  addSchedule First UpdateSchedule
  addSchedule PreUpdate UpdateSchedule
  addSchedule Update UpdateSchedule
  addSchedule PostUpdate UpdateSchedule

  addScheduleEdge (First, PreUpdate) UpdateSchedule
  addScheduleEdge (PreUpdate, Update) UpdateSchedule
  addScheduleEdge (Update, PostUpdate) UpdateSchedule

register :: forall c. (Runnable c) => Plugin ()
register = run $ runFor @c registerComponent

registerComponent :: forall c. (Component c) => Proxy c -> System ()
registerComponent c = do
  _ <- getOrAddComponentId (ComponentType c)
  return ()

module MischiefECS.App where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map
import Data.Map qualified as Map
import MischiefECS.App.Scheduler (ScheduleType (..), Scheduler)
import MischiefECS.App.Scheduler qualified as Scheduler
import MischiefECS.App.Schedules
import MischiefECS.App.SystemConfig
import MischiefECS.App.Systems (Systems)
import MischiefECS.App.Systems qualified as Systems
import MischiefECS.Components
import MischiefECS.Events
import MischiefECS.World
import MischiefECS.World.Insert
import MischiefECS.World.Spawn

data App = App
  { world :: World,
    systems :: Systems,
    scheduler :: Scheduler
  }

type Plugin = ReaderT App IO

newApp :: [Plugin ()] -> IO App
newApp plugins = do
  world <- newWorld
  systems <- Systems.newSystems
  scheduler <- Scheduler.newScheduler

  let app = App {world, systems, scheduler}

  for_ (appInit : plugins) $ \plugin ->
    runReaderT plugin app

  return app

runApp :: App -> IO ()
runApp app = do
  startups <- Scheduler.getSchedules StartupSchedule app.scheduler
  updates <- Scheduler.getSchedules UpdateSchedule app.scheduler

  runSchedules startups
  runSchedulesLoop updates
  where
    runSchedulesLoop schedules = do
      forever $ do
        runSchedules schedules
        modifyIORef' app.world.frame (\(Frame x) -> Frame $ x + 1)

    runSchedules schedules =
      for_ schedules $ \schedule -> do
        systems <- Scheduler.getScheduleSystems schedule app.scheduler

        for_ (concat systems) $ \systemId -> do
          (system, systemTick) <- Systems.getSystemData systemId app.systems

          lastSystemTick <- readIORef systemTick
          currentSystemTick <- readIORef app.world.tick
          writeIORef systemTick currentSystemTick

          let world = setSystemId systemId $ setSystemTicks lastSystemTick currentSystemTick app.world

          runReaderT system world
          runReaderT flush world
          runReaderT flushEvents world
          runReaderT tick world

addSystems :: (Schedule sc, SystemConfig s) => sc -> s -> Plugin ()
addSystems schedule system = do
  app <- ask
  let label = ScheduleLabel $ typeOf schedule
  let SystemConfigData {systems, edges} = systemConfigData system

  for_ systems $ \system -> do
    systemId <- liftIO $ Systems.getSystemId label system app.systems
    liftIO $ Scheduler.addSystem label systemId app.scheduler

  for_ edges $ \(s1, s2) -> do
    id1 <- liftIO $ Systems.getSystemId label s1 app.systems
    id2 <- liftIO $ Systems.getSystemId label s2 app.systems
    liftIO $ Scheduler.addSystemEdge label (id1, id2) app.scheduler

addSchedule :: (Schedule s) => s -> ScheduleType -> Plugin ()
addSchedule schedule scheduleType = do
  App {scheduler} <- ask
  liftIO $ Scheduler.addSchedule (ScheduleLabel $ typeOf schedule) scheduleType scheduler

addScheduleEdge :: (Schedule s1, Schedule s2) => (s1, s2) -> ScheduleType -> Plugin ()
addScheduleEdge (s1, s2) scheduleType = do
  App {scheduler} <- ask
  liftIO $ Scheduler.addScheduleEdge (ScheduleLabel $ typeOf s1, ScheduleLabel $ typeOf s2) scheduleType scheduler

addResource :: (Component r, Storage r ~ ResourceStorage) => r -> Plugin ()
addResource r = do
  app <- ask
  liftIO $ runReaderT (insertResource r) app.world

addObserver :: (Event e) => (e -> System ()) -> Plugin ()
addObserver observer = do
  app <- ask
  liftIO $ runReaderT (spawnObserver $ Observer observer) app.world

addPlugin :: Plugin () -> Plugin ()
addPlugin plugin = do
  app <- ask
  liftIO $ runReaderT plugin app

data Schedules = Schedules {startup :: IORef [TypeRep], update :: IORef [TypeRep]}

newSchedules :: IO Schedules
newSchedules = do
  startup <- newIORef [typeOf Startup]
  update <- newIORef [typeOf PreUpdate, typeOf Update, typeOf PostUpdate]
  return Schedules {startup, update}

appInit :: Plugin ()
appInit = do
  addScheduleEdge (PreUpdate, Update) UpdateSchedule
  addScheduleEdge (Update, PostUpdate) UpdateSchedule

  addSchedule Startup StartupSchedule

  addSchedule PreUpdate UpdateSchedule
  addSchedule Update UpdateSchedule
  addSchedule PostUpdate UpdateSchedule

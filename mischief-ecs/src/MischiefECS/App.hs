module MischiefECS.App where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map
import Data.Map qualified as Map
import GHC.Base (runRW#)
import MischiefECS.Components
import MischiefECS.Events
import MischiefECS.World

data App = App {systems :: IORef (Map TypeRep [(SystemId, System (), IORef Tick)]), world :: World, schedules :: Schedules, systemCounter :: IORef Int}

type Plugin = ReaderT App IO

newApp :: [Plugin ()] -> IO App
newApp plugins = do
  world <- newWorld
  systems <- newIORef Map.empty
  schedules <- newSchedules
  systemCounter <- newIORef 0

  let app = App {systems, world, schedules, systemCounter}

  for_ plugins $ \plugin ->
    runReaderT plugin app

  return app

runApp :: App -> IO ()
runApp app = do
  systemMap <- readIORef app.systems

  startups <- readIORef app.schedules.startup
  updates <- readIORef app.schedules.update

  runSchedules startups systemMap
  runSchedulesLoop updates systemMap
  where
    runSchedulesLoop schedules systemMap = do
      forever $ do
        runSchedules schedules systemMap
        modifyIORef' app.world.frame (\(Frame x) -> Frame $ x + 1)

    runSchedules schedules systemMap =
      for_ schedules $ \schedule -> do
        case Map.lookup schedule systemMap of
          Nothing -> return ()
          Just systems -> do
            for_ systems $ \(systemId, system, systemTick) -> do
              lastSystemTick <- readIORef systemTick
              currentSystemTick <- readIORef app.world.tick
              writeIORef systemTick currentSystemTick

              let world = setSystemId systemId $ setSystemTicks lastSystemTick currentSystemTick app.world

              runReaderT system world
              runReaderT flush world
              runReaderT flushEvents world
              runReaderT tick world

addSystem :: (Schedule s) => s -> System () -> Plugin ()
addSystem schedule system = do
  app <- ask
  t0 <- liftIO $ newIORef $ Tick 0
  t1 <- liftIO $ newIORef $ Tick 0
  systemId <- liftIO $ readIORef app.systemCounter
  liftIO $ modifyIORef' app.systemCounter (+ 1)

  liftIO $ modifyIORef' app.systems (Map.alter (alterMap t0 t1 (SystemId systemId)) (typeOf schedule))
  where
    alterMap t0 _ systemId Nothing = Just [(systemId, system, t0)]
    alterMap _ t1 systemId (Just l) = Just $ l ++ [(systemId, system, t1)]

addSystems :: (Schedule s) => s -> [System ()] -> Plugin ()
addSystems schedule systems = for_ systems (addSystem schedule)

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

class (Typeable s) => Schedule s

data Startup = Startup deriving (Schedule)

data Update = Update deriving (Schedule)

data PreUpdate = PreUpdate deriving (Schedule)

data PostUpdate = PostUpdate deriving (Schedule)

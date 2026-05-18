module MischiefECS.App where

import Control.Monad (forM, forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map
import Data.Map qualified as Map
import MischiefECS.World

data App = App {systems :: IORef (Map TypeRep [System ()]), world :: World, schedules :: Schedules}

type Plugin = ReaderT App IO

newApp :: [Plugin ()] -> IO App
newApp plugins = do
  world <- newWorld
  systems <- newIORef Map.empty
  schedules <- newSchedules

  let app = App {systems, world, schedules}

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
      forever $ runSchedules schedules systemMap

    runSchedules schedules systemMap =
      for_ schedules $ \schedule -> do
        case Map.lookup schedule systemMap of
          Nothing -> return ()
          Just systems -> do
            for_ systems $ \system ->
              runReaderT system app.world

addSystem :: (Schedule s) => s -> System () -> Plugin ()
addSystem schedule system = do
  app <- ask
  liftIO $ modifyIORef' app.systems (Map.alter alterMap (typeOf schedule))
  where
    alterMap Nothing = Just [system]
    alterMap (Just l) = Just $ l ++ [system]

addPlugin :: Plugin () -> Plugin ()
addPlugin plugin = do
  app <- ask
  liftIO $ runReaderT plugin app

data Schedules = Schedules {startup :: IORef [TypeRep], update :: IORef [TypeRep]}

newSchedules :: IO Schedules
newSchedules = do
  startup <- newIORef [typeOf Startup]
  update <- newIORef [typeOf Update]
  return Schedules {startup, update}

class (Typeable s) => Schedule s

data Startup = Startup deriving (Schedule)

data Update = Update deriving (Schedule)

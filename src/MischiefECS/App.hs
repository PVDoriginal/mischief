module MischiefECS.App where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.IORef
import MischiefECS.World

data App = App {systems :: IORef [System ()], world :: World}

type Plugin = ReaderT App IO

newApp :: [Plugin ()] -> IO App
newApp plugins = do
  world <- newWorld
  systems <- newIORef []
  let app = App {systems, world}

  for_ plugins $ \plugin ->
    runReaderT plugin app

  return app

runApp :: App -> IO ()
runApp app = do
  systems <- readIORef app.systems
  for_ systems $ \system ->
    runReaderT system app.world

addSystem :: System () -> Plugin ()
addSystem system = do
  app <- ask
  liftIO $ modifyIORef' app.systems (++ [system])

addPlugin :: Plugin () -> Plugin ()
addPlugin plugin = do
  app <- ask
  liftIO $ runReaderT plugin app
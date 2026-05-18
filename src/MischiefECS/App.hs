module MischiefECS.App where

import Control.Monad (forM)
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.IORef
import MischiefECS.World

data App = App {systems :: IORef [System ()], world :: World}

newApp :: IO App
newApp = do
  world <- newWorld
  systems <- newIORef []
  return App {systems, world}

addSystem :: System () -> App -> IO ()
addSystem system App {systems} = do
  modifyIORef' systems (++ [system])

runApp :: App -> IO ()
runApp app = do
  systems <- readIORef app.systems
  for_ systems $ \system ->
    runReaderT system app.world

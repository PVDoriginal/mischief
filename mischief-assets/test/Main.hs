module Main where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Data
import Data.Store
import GHC.Generics
import MischiefAssets
import MischiefECS
import MischiefECS.Components.Spawn
import System.IO

data Test = Test String Int deriving (Generic, Store, Show)

instance Asset Test where
  loadAsset :: ByteString -> Test
  loadAsset = decodeEx

  extensions :: [String]
  extensions = ["a"]

main :: IO ()
main = do
  B.writeFile "assets/c.a" (encode $ Test "test string" 5)
  app <- newApp [assetPlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  registerAsset @Test
  addSystems Startup setup
  addSystems Update dummy
  addObserver onLoad

setup :: System ()
setup = do
  void $ load "assets/c.a"

onLoad :: OnLoad Test -> System ()
onLoad event = do
  Just c <- get @(AssetData Test) event.entity
  liftIO $ print c

dummy :: System ()
dummy = return ()
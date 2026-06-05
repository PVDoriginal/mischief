module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable hiding (and, or)
import Data.Maybe
import MischiefECS

data C = C deriving (Component, Queryable)

main :: IO ()
main = do
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup setup

setup :: System ()
setup = do
  par
    [ for_ [0 .. 100 :: Int] $ \i -> do
        liftIO $ threadDelay 10000
        liftIO $ print i,
      for_ [0 .. 100 :: Int] $ \i -> do
        liftIO $ threadDelay 10000
        liftIO $ print i
    ]
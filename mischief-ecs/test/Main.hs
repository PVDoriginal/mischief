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
  app <- newApp [timePlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Update update

update :: System ()
update = do
  Just time <- single @Time
  liftIO $ print time.value.deltaSecs
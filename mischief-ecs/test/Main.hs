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
  addSystems Update update

setup :: System ()
setup = do
  par
    [ do
        _ <- spawnDefer C
        return (),
      do
        _ <- spawnDefer C
        return (),
      do
        _ <- spawnDefer C
        return ()
    ]

update :: System ()
update = do
  parIter' @Entity (added @C) $ \entity -> do
    defer $ insert C entity
    liftIO $ print entity

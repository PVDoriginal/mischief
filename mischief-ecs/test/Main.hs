module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable hiding (and, or)
import Data.Maybe
import MischiefECS
import System.Exit (exitSuccess)
import Prelude hiding (and)

data Parent1 = Parent1 deriving (Component, Queryable)

data Parent2 = Parent2 deriving (Component, Queryable)

data ChildOf = ChildOf deriving (Component, Queryable)

main :: IO ()
main = do
  app <- newApp [timePlugin, plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup setup
  addSystems Startup $ exit `after` setup

setup :: System ()
setup = do
  p1 <- spawn Parent1
  p2 <- spawn Parent2

  _ <- spawn (R (ChildOf, p1), Name "Child of p1")
  _ <- spawn (R (ChildOf, p1), Name "Child of p1")
  _ <- spawn (R (ChildOf, p1), Name "Child of p1")

  _ <- spawn (R (ChildOf, p2), Name "Child of p2")
  _ <- spawn (R (ChildOf, p2), Name "Child of p2")

  q1 <- query' @Name $ withR @ChildOf p1
  q2 <- query' @Name $ withR @ChildOf p2
  q3 <- query' @Name $ with @Parent1

  liftIO $ print "p1 children:"
  for_ q1 $ \name ->
    liftIO $ print name

  liftIO $ print "p2 children:"
  for_ q2 $ \name ->
    liftIO $ print name

  liftIO $ print "p1:"
  for_ q3 $ \name ->
    liftIO $ print name

exit :: System ()
exit = do
  liftIO exitSuccess
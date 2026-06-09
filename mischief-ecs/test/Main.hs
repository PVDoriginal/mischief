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

data ChildOf = ChildOf deriving (Component, Queryable, Show)

main :: IO ()
main = do
  app <- newApp [plugin]
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
  _ <- spawn ((R (ChildOf, p2), R (ChildOf, p1)), Name "Child of p1 and p2")

  q4 <- query @(R ChildOf)
  for_ q4 $ \result ->
    for_ result.collection $ \rel -> do
      liftIO $ putStrLn $ show rel.entity ++ " is child of " ++ show rel.target

exit :: System ()
exit = do
  liftIO exitSuccess
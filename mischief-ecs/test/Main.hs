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

main :: IO ()
main = do
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  -- addRelationshipWithSettings @ChildOf RelationshipSettings {exclusivity = Exclusive}
  addSystems Startup setup
  addSystems Startup $ exit `after` setup

-- addObserver observer

setup :: System ()
setup = do
  p1 <- spawn (Parent1, Name "Parent 1")
  p2 <- spawn (Parent2, Name "Parent 2")
  return ()

-- _ <- spawn (R (ChildOf, p1), Name "Child 1")
-- _ <- spawn (R (ChildOf, p1), Name "Child 2")
-- _ <- spawn (R (ChildOf, p1), Name "Child 3")

-- _ <- spawn (R (ChildOf, p2), Name "Child 4")
-- _ <- spawn ((R (ChildOf, p2), R (ChildOf, p1)), Name "Child 5")

-- q4 <- query @(Name, R ChildOf)
-- for_ q4 $ \(name, result) ->
--   for_ result.collection $ \rel -> do
--     liftIO $ putStrLn $ show name ++ " is child of " ++ show rel.target
--     Just parentName <- get @Name rel.target
--     liftIO $ print parentName

-- observer :: OnInsertR ChildOf -> System ()
-- observer event = liftIO $ putStrLn $ show event.entity ++ " is now a child of " ++ show event.target

exit :: System ()
exit = do
  liftIO exitSuccess
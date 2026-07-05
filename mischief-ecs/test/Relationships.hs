{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Relationships where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable hiding (and, or)
import Data.Maybe
import GHC.Exts
import MischiefECS
import MischiefECS.Components.Spawn (meta)
import System.Exit (exitSuccess)
import Prelude hiding (and)

data Parent1 = Parent1 deriving (Component, Queryable)

data Parent2 = Parent2 deriving (Component, Queryable)

data A = A {lol :: Int, lol2 :: C}

type A# = (# Int#, C# #)

data C = C Float Int

type C# = (# Float#, Int# #)

testRelationships :: IO ()
testRelationships = do
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup setup
  addSystems Startup $ exit `after` setup
  addObserver observer

setup :: System ()
setup = do
  p1 <- spawn (Parent1, Name "Parent 1")
  p2 <- spawn (Parent2, Name "Parent 2")

  _ <- spawn (Rel (ChildOf, p1), Name "Child 1")
  _ <- spawn (Rel (ChildOf, p1), Name "Child 2")
  _ <- spawn (Rel (ChildOf, p1), Name "Child 3")

  _ <- spawn (Rel (ChildOf, p2), Name "Child 4")
  _ <- spawn ((Rel (ChildOf, p2), Rel (ChildOf, p1)), Name "Child 5")

  q4 <- query @(Name, Rel ChildOf)
  for_ q4 $ \(name, result) ->
    for_ result $ \rel -> do
      liftIO $ putStrLn $ show name ++ " is child of " ++ show (target rel)
      Just parentName <- get @Name (target rel)
      liftIO $ print parentName

observer :: OnInsertRel ChildOf -> System ()
observer event = liftIO $ putStrLn $ show event.entity ++ " is now a child of " ++ show event.target

exit :: System ()
exit = do
  liftIO exitSuccess
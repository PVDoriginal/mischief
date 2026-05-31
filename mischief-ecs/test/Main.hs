module Main where

import Control.Monad.IO.Class
import Data.Foldable hiding (and, or)
import MischiefECS
import MischiefECS.App.Scheduler
import MischiefECS.Graph

newtype Msg = Msg String deriving (Message, Show)

main :: IO ()
main = do
  -- app <- newApp [plugin]
  -- runApp app
  testGraph

plugin :: Plugin ()
plugin = do
  addMessage @Msg
  addSystem Update writer
  addSystem Update reader1
  addSystem Update reader2

writer :: System ()
writer = do
  Just msg <- single @(Messages Msg)

  writeMessage (Msg "M1") msg
  writeMessage (Msg "M2") msg
  writeMessage (Msg "M3") msg

reader1 :: System ()
reader1 = do
  Just msg <- single @(Messages Msg)
  messages <- readMessages msg
  liftIO $ putStrLn $ "reader1: " ++ show messages

reader2 :: System ()
reader2 = do
  Just msg <- single @(Messages Msg)
  messages <- readMessages msg
  liftIO $ putStrLn $ "reader2: " ++ show messages

testGraph :: IO ()
testGraph = do
  graph <- newGraph
  for_ [0 :: Int .. 10] $ \i -> addNode i graph

  addEdge (0, 1) graph
  addEdge (0, 2) graph
  addEdge (0, 3) graph

  addEdge (2, 4) graph
  addEdge (2, 3) graph

  addEdge (3, 10) graph

  nodes <- getNodes graph
  for_ nodes $ \nodes ->
    print nodes

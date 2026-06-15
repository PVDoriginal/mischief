module Main where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import MischiefECS
import System.IO

data Dummy = Dummy ByteString deriving (Component, Queryable)

main :: IO ()
main = do
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  addSystems Startup printString

printString :: System ()
printString = do
  liftIO $ print "AAA"
  contents <- liftIO $ async $ B.readFile "a.txt"
  -- _ <- spawn (Dummy contents)
  liftIO $ print "BBB"

-- liftIO $ print contents

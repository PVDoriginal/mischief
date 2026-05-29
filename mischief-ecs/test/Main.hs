module Main where

import Control.Monad.IO.Class
import Data.Foldable hiding (and, or)
import Data.List qualified as List
import Data.Set qualified as Set
import GHC.Generics (Generic)
import MischiefECS

newtype Msg = Msg String deriving (Message, Show)

main :: IO ()
main = do
  app <- newApp [plugin]
  runApp app

plugin :: Plugin ()
plugin = do
  registerMessage @Msg
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
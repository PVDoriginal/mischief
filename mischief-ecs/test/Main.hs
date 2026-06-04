module Main where

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
  addSystems Startup $ do
    _ <- spawn C
    return ()

  addSystems PreUpdate s1

  addSystems Update $ s4 `before` s2
  addSystems Update s2
  addSystems PostUpdate s3

s1 :: System ()
s1 = do
  single <- single' @Entity $ added @C
  when (isJust single) $ do
    liftIO $ print "1"

s2 :: System ()
s2 = do
  single <- single' @Entity $ added @C
  when (isJust single) $ do
    liftIO $ print "2"

s3 :: System ()
s3 = do
  single <- single' @Entity $ added @C
  when (isJust single) $ do
    liftIO $ print "3"

s4 :: System ()
s4 = do
  single <- single' @Entity $ added @C
  when (isJust single) $ do
    liftIO $ print "4"

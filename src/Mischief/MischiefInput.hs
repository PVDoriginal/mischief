module MischiefInput where

import Control.Monad.IO.Class
import MischiefECS
import System.IO

data Key = Space | Enter deriving (Show, Eq)

inputPlugin :: Plugin ()
inputPlugin = do
  liftIO $ hSetBuffering stdin NoBuffering

readInput :: System ()
readInput = do
  undefined

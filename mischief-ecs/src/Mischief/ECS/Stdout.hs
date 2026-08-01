module Mischief.ECS.Stdout where

import Control.Monad.IO.Class
import Mischief.ECS.World
import System.Console.ANSI

printClear :: String -> System ()
printClear s = liftIO $ do
  liftIO clearScreen
  liftIO $ setCursorPosition 0 0
  liftIO $ putStr s
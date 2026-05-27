module MischiefInput where

import Control.Monad
import Control.Monad.IO.Class
import MischiefECS
import System.IO

data Key = Space | Enter | UnknownKey deriving (Show, Eq)

data KeyInput = KeyInput
  { justPressed :: [Key],
    pressed :: [Key]
  }

inputPlugin :: Plugin ()
inputPlugin = do
  liftIO $ hSetBuffering stdin NoBuffering
  addSystem Update readInput

readInput :: System ()
readInput = do
  ready <- liftIO $ hReady stdin
  when ready $ do
    c <- liftIO getChar
    liftIO $ print c

mapKey :: Char -> Key
mapKey c =
  case c of
    ' ' -> Space
    '\n' -> Enter
    _ -> UnknownKey
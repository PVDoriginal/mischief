module Mischief.ECS.Stdin where

import Control.Monad.IO.Class
import GHC.IO.Handle
import Mischief.ECS.World
import System.IO
import Prelude hiding (read)

init :: System ()
init = liftIO $ do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

read :: System String
read = do
  ready <- liftIO $ hReady stdin
  if ready
    then do
      c <- liftIO getChar
      ([c] ++) <$> read
    else do
      return []

readLast :: System (Maybe Char)
readLast = do
  r <- read
  return $ case r of
    [] -> Nothing
    x -> Just $ last x
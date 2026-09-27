module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable hiding (and)
import GHC.Generics hiding (C, C1)
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.Input
import Mischief.Input.Keys (Keys)
import Mischief.Input.Keys qualified as Keys
import Prelude hiding (and)

main :: IO ()
main = do
  app <- newApp
  addPlugin @MainPlugin app
  runApp app

data MainPlugin

instance Plugin MainPlugin where
  init = do
    schedule @Update $ systems test
  deps = [dep @InputPlugin]

test :: System ()
test = do
  Just keys <- res @Keys
  when (Keys.justPressed keys Keys.A) $ do
    info "AAAA"

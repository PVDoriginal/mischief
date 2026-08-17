module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable hiding (and)
import GHC.Generics hiding (C, C1)
import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.Input.Keyboard
import SDL3.Sys qualified as SDL3
import Prelude hiding (and)

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    Systems.add Update test
  plugins _ = plug KeyboardPlugin

test :: System ()
test = do
  Just keys <- res @Keys
  when (justPressed SDL3.SDL_SCANCODE_A keys) $ do
    info "AAAA"
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
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    Systems.add Update test
  plugins _ = plug InputPlugin

test :: System ()
test = do
  Just keys <- res @Keys
  when (Keys.justPressed Keys.A keys) $ do
    info "AAAA"

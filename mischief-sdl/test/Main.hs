module Main where

import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.SDL (SDLPlugin (..))

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    Systems.add Update (pure () :: System ())

  plugins _ = plug SDLPlugin
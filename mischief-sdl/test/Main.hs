module Main where

import Mischief.ECS.Prelude
import Mischief.ECS.Systems qualified as Systems
import Mischief.SDL (SDLPlugin (..))
import Mischief.SDL.Window

main :: IO ()
main = do
  app <- newApp MainPlugin
  runApp app

data MainPlugin = MainPlugin deriving (Eq)

instance Plugin MainPlugin where
  init _ = do
    Systems.add Update (pure () :: System ())
    Systems.add Startup setup

  plugins _ = plug SDLPlugin

setup :: System ()
setup = do
  w1 <- spawn (Window, WindowSize 100 300)
  w2 <- spawn (Window, WindowTitle "Second Window", WindowSize 1000 40)
  w3 <- spawn Window
  pure ()
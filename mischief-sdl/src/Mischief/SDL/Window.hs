module Mischief.SDL.Window where

import Control.Monad.IO.Class
import Data.Default
import Foreign.C
import Foreign.C.ConstPtr (ConstPtr (..))
import Mischief.ECS (Collectable (collect), hook)
import Mischief.ECS.Components.HooksDef (HookContext (..))
import Mischief.ECS.Events
import Mischief.ECS.Prelude
import Mischief.SDL
import SDL3.Sys qualified as SDL3

data Window = Window

instance Component Window where
  onAdd = [hook handleNewWindow]
  onRemove = [hook handleWindowRemove]
  required = require @(WindowSize, WindowTitle)

data WindowSize = WindowSize {width :: Int, height :: Int} deriving (Component)

instance Default WindowSize where
  def = WindowSize 600 400

newtype WindowTitle = WindowTitle String deriving anyclass (Component)

instance Default WindowTitle where
  def = WindowTitle "Mischief Window"

handleNewWindow :: HookContext -> System ()
handleNewWindow (HookContext entity) = do
  Just (WindowSize w h, WindowTitle title) <- [g|*WindowSize, *WindowTitle|] entity
  window <- liftIO $ withCString title $ \title -> SDL3.createWindow (ConstPtr title) (fromIntegral w) (fromIntegral h) 0
  insert (SDLWindow window) entity

handleWindowRemove :: HookContext -> System ()
handleWindowRemove (HookContext entity) = do
  Just (SDLWindow p) <- [g|*SDLWindow|] entity
  liftIO $ SDL3.destroyWindow p
module Mischief.ECS.App.SystemDef where

import Data.Default
import GHC.Generics
import Mischief.ECS.Components

newtype SystemTick = SystemTick {inner :: Tick}
  deriving stock (Generic)
  deriving anyclass (Component, Default)

newtype LastSystemTick = LastSystemTick {inner :: Tick}
  deriving stock (Generic)
  deriving anyclass (Component, Default)

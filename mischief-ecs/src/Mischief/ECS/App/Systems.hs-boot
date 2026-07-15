module Mischief.ECS.App.Systems where

import Mischief.ECS.Components
import Mischief.ECS.Entities
import Mischief.ECS.Utils
import Mischief.ECS.World

getSystemTicks :: World -> IO (Tick, Tick)

newtype SystemFunction = SystemFunction {inner :: System ()}

instance Component SystemFunction

newtype SystemTick = SystemTick {inner :: Tick}

instance Component SystemTick

newtype LastSystemTick = LastSystemTick {inner :: Tick}

instance Component LastSystemTick

self :: forall m w. (MonadSystem w m) => m Entity
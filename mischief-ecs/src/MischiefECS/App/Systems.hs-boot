module MischiefECS.App.Systems where

import MischiefECS.Components
import MischiefECS.Entities
import MischiefECS.World
import MischiefECS.World.Internal

getSystemTicks :: World -> IO (Tick, Tick)

newtype SystemFunction = SystemFunction {inner :: System ()}

instance Component SystemFunction

newtype SystemTick = SystemTick {inner :: Tick}

instance Component SystemTick

newtype LastSystemTick = LastSystemTick {inner :: Tick}

instance Component LastSystemTick

localEntity :: forall m w. (MonadSystem w m) => m Entity
module Mischief.ECS.Observers where

import Mischief.ECS.Entities
import Mischief.ECS.EventDef
import Mischief.ECS.Observer
import Mischief.ECS.World
import Mischief.ECS.World.Spawn qualified

spawn :: (Event e) => (e -> System ()) -> System Entity
spawn x = Mischief.ECS.World.Spawn.spawn $ Observer x
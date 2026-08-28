module Mischief.Assets.Plugin where

import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import Mischief.Assets.Asset
import Mischief.Assets.Image
import Mischief.Assets.Register
import Mischief.ECS.App
import Mischief.ECS.Components
import Mischief.ECS.World.Query

module MischiefAssets.Plugin where

import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import Mischief.ECS.App
import Mischief.ECS.Components
import Mischief.ECS.World.Query
import MischiefAssets.Asset
import MischiefAssets.Image
import MischiefAssets.Register

assetPlugin :: Plugin ()
assetPlugin = do
  initRes @Extensions
  registerAsset @Image
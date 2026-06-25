module MischiefAssets.Plugin where

import Data.Default
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import MischiefAssets.Asset
import MischiefAssets.Image
import MischiefAssets.Register
import MischiefECS.App
import MischiefECS.Components
import MischiefECS.World.Query

assetPlugin :: Plugin ()
assetPlugin = do
  initRes @Extensions
  registerAsset @Image
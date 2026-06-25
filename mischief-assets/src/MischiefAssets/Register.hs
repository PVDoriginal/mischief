{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefAssets.Register where

import Data.Data
import Data.Foldable
import MischiefAssets.Asset
import MischiefECS

registerAsset :: forall a. (Asset a) => Plugin ()
registerAsset = run $ do
  Just ext <- res @Extensions

  let exts = extensions @a
  let t = AssetType $ Proxy @a

  for_ exts $ \extension -> do
    modify ext $ registerExtension extension t
{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.Assets.Asset where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Data
import Mischief.ECS.Prelude

class (Typeable a, Component a) => Asset a where
  loadAsset :: ByteString -> IO a

data Loading = Loading deriving (Component)

data Loaded = Loaded deriving (Component)

data AssetEntity = AssetEntity deriving (Component)

load :: forall a. (Asset a, Bundle a) => FilePath -> System Entity
load !path = do
  entity <- spawn (AssetEntity, Loading)

  runAfter
    ( do
        contents <- BS.readFile path
        !asset' <- loadAsset @a contents
        return asset'
    )
    ( \asset -> do
        remove (C @Loading) entity
        insert (asset, (AssetType $ Proxy @a, Loaded)) entity

        trigger (OnLoad @a entity)
    )

  return entity

newtype OnLoad a = OnLoad {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

data AssetType where
  AssetType :: (Asset a) => Proxy a -> AssetType
  deriving (Component)
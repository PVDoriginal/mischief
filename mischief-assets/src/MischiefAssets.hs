{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefAssets where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Data
import GHC.IO.Handle
import MischiefECS

class (Typeable a) => Asset a where
  loadAsset :: ByteString -> a

newtype AssetData a = AssetData a deriving anyclass (Component, Queryable)

data Loading = Loading deriving (Component, Queryable)

data Loaded = Loaded deriving (Component, Queryable)

data AssetEntity = AssetEntity deriving (Component, Queryable)

load :: forall a. (Asset a) => FilePath -> System Entity
load !path = do
  entity <- spawn (AssetEntity, Loading)

  forkSystem $ do
    asset <- liftIO $ B.readFile path
    let !asset' = loadAsset @a asset
    defer $ do
      remove @Loading entity
      insert (AssetData asset', Loaded) entity

  return entity

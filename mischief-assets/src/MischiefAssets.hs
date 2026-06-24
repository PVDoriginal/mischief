{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefAssets where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Data
import Data.Kind
import GHC.IO.Handle
import MischiefECS

class (Typeable a) => Asset a where
  loadAsset :: ByteString -> a

newtype AssetData a = AssetData a
  deriving anyclass (Component, Queryable)
  deriving stock (Show)

data Loading = Loading deriving (Component, Queryable)

data Loaded = Loaded deriving (Component, Queryable)

data AssetEntity = AssetEntity deriving (Component, Queryable)

load :: forall a. (Asset a) => FilePath -> System Entity
load !path = do
  entity <- spawn (AssetEntity, Loading)

  runAfter
    ( do
        asset <- liftIO $ B.readFile path
        let !asset' = loadAsset @a asset
        return asset'
    )
    ( \asset -> do
        remove @Loading entity
        insert (AssetData asset, (AssetType $ Proxy @a, Loaded)) entity

        let event :: OnLoad a = OnLoad entity
        trigger event
    )

  return entity

data AssetType where
  AssetType :: forall (a :: Type). (Asset a) => (Proxy a) -> AssetType
  deriving (Component, Queryable)

instance GetRep AssetType where
  getRep :: AssetType -> TypeRep
  getRep (AssetType (_ :: Proxy a)) = typeRep $ Proxy @a

newtype OnLoad a = OnLoad {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)
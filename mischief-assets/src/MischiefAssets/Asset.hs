{-# LANGUAGE AllowAmbiguousTypes #-}

module MischiefAssets.Asset where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Data
import Data.Default
import Data.Kind
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics
import GHC.IO.Handle
import MischiefECS

class (Typeable a) => Asset a where
  loadAsset :: ByteString -> a
  extensions :: [String]

newtype AssetData a = AssetData a
  deriving anyclass (Component, Queryable)
  deriving stock (Show)

data Loading = Loading deriving (Component, Queryable)

data Loaded = Loaded deriving (Component, Queryable)

data AssetEntity = AssetEntity deriving (Component, Queryable)

loadWithType :: forall a. (Asset a) => FilePath -> System Entity
loadWithType !path = do
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

load :: FilePath -> System Entity
load !path = do
  t <- getTypeFromPath path
  loadInner t
  where
    loadInner (AssetType (_ :: Proxy a)) = loadWithType @a path

data AssetType where
  AssetType :: forall (a :: Type). (Asset a) => (Proxy a) -> AssetType
  deriving (Component, Queryable)

instance Show AssetType where
  show = show . getRep

instance GetRep AssetType where
  getRep :: AssetType -> TypeRep
  getRep (AssetType (_ :: Proxy a)) = typeRep $ Proxy @a

newtype OnLoad a = OnLoad {entity :: Entity}
  deriving anyclass (Event)
  deriving stock (Show)

newtype Extensions = Extensions {inner :: Map String AssetType}
  deriving stock (Generic, Show)
  deriving anyclass (Component, Queryable)
  deriving newtype (Default)

registerExtension :: String -> AssetType -> Extensions -> Extensions
registerExtension ext t extensions =
  case Map.lookup ext extensions.inner of
    Just _ -> undefined
    Nothing -> Extensions $ Map.insert ext t extensions.inner

getTypeFromPath :: FilePath -> System AssetType
getTypeFromPath !path = do
  let ext = drop 1 . dropWhile (/= '.') $ path
  Just extensions <- res @Extensions
  maybe undefined return (Map.lookup ext extensions.value.inner)
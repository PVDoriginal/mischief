module Mischief.Render.Shader.Structure where

import Data.Data
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits

data RawStructure = StructRoot Text RawStructure | StructConcat RawStructure RawStructure | StructField Field | StructNone deriving (Show)

data Field = Field {fieldName :: Text, fieldType :: Text} deriving (Show)

data Structure = Structure Text [Field] deriving (Show)

class (Typeable a) => Structable a where
  getStruct :: Proxy a -> RawStructure
  default getStruct :: (Structable' (Rep a)) => Proxy a -> RawStructure
  getStruct _ = getStruct' (Proxy @(Rep a))

class Structable' f where
  getStruct' :: Proxy f -> RawStructure

instance Structable' V1 where
  getStruct' _ = StructNone

instance Structable' U1 where
  getStruct' _ = StructNone

instance (Typeable t, KnownSymbol name) => Structable' (S1 (MetaSel (Just name) w' w'' w''') (K1 n t)) where
  getStruct' _ = StructField $ Field (T.pack $ symbolVal $ Proxy @name) (T.pack $ show $ typeRep (Proxy @t))

instance (Structable' f, Structable' g) => Structable' (f :*: g) where
  getStruct' _ = StructConcat (getStruct' (Proxy @f)) (getStruct' (Proxy @g))

instance (Structable' c) => Structable' (C1 i c) where
  getStruct' _ = getStruct' (Proxy @c)

instance (KnownSymbol name, Structable' i) => Structable' (D1 (MetaData name w w' w'') i) where
  getStruct' _ = StructRoot (T.pack $ symbolVal (Proxy @name)) (getStruct' (Proxy @i))

data BufferTest = BufferText
  { a :: Int,
    b :: Float
  }
  deriving (Generic, Structable)

processFields :: RawStructure -> [Field]
processFields StructNone = []
processFields (StructConcat a b) = processFields a ++ processFields b
processFields (StructField field) = [field]
processFields _ = undefined

processStruct :: RawStructure -> Structure
processStruct (StructRoot text struct) = Structure text (processFields struct)
processStruct _ = undefined

test = processStruct $ getStruct $ Proxy @BufferTest
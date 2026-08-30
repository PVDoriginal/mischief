module Mischief.Render.Shader.Buffers where

import Data.Data
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as VS
import Foreign (Word8)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Mischief.Render.Shader.Types (Vec4f)

data Structure = StructRoot Text [Structure] | StructField Field | StructNone deriving (Show)

data Field = Field Text Text deriving (Show)

class (Typeable a) => Bufferable a where
  bufferToWgsl :: Proxy a -> Structure
  default bufferToWgsl :: (Bufferable' (Rep a)) => Proxy a -> Structure
  bufferToWgsl _ = bufferToWgsl' (Proxy @(Rep a))

class Bufferable' f where
  bufferToWgsl' :: Proxy f -> Structure

instance Bufferable' V1 where
  bufferToWgsl' _ = StructNone

instance Bufferable' U1 where
  bufferToWgsl' _ = StructNone

instance (Bufferable' f, Bufferable' g) => Bufferable' (f :*: g) where
  bufferToWgsl' _ = StructNone

instance (Bufferable c) => Bufferable' (K1 i c) where
  bufferToWgsl' _ = StructNone

-- instance Bufferable' (M1 S (MetaSel s w w' w'') (K1 i c)) where
--   bufferToWgsl' = ""

instance (KnownSymbol name) => Bufferable' (D1 (MetaData name w w' w'') i) where
  bufferToWgsl' _ = StructRoot (T.pack $ symbolVal (Proxy @name)) []

-- instance (Bufferable' f) => Bufferable' (M1 i t f) where
--   bufferToWgsl' _ = StructNone

type family BufferType a where
  BufferType Vec4f = (Float, Float, Float, Float)

class ConvertBuffer gpu cpu | gpu -> cpu where
  convertBuffer :: gpu -> cpu

data BufferTest = BufferText
  {
  }
  deriving (Generic, Bufferable)

test = bufferToWgsl $ Proxy @BufferTest
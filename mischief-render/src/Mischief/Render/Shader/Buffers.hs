module Mischief.Render.Shader.Buffers where

import Data.Data
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as VS
import Foreign (Word8)
import GHC.Generics
import Mischief.Render.Shader.Types (Vec4f)

class (Typeable a) => Buffer a where
  bytes :: a -> Vector Word8
  default bytes :: (Buffer' (Rep a), Generic a) => a -> Vector Word8
  bytes a = bytes' (from a)

class Buffer' f where
  bytes' :: f p -> Vector Word8

instance Buffer' V1 where
  bytes' _ = VS.empty

instance Buffer' U1 where
  bytes' _ = VS.empty

instance (Buffer' f, Buffer' g) => Buffer' (f :*: g) where
  bytes' (f :*: g) = bytes' f <> bytes' g

instance (Buffer c) => Buffer' (K1 i c) where
  bytes' (K1 x) = bytes x

instance (Buffer' f) => Buffer' (M1 i t f) where
  bytes' (M1 x) = bytes' x

instance Buffer Vec4f where
  bytes _ = undefined

module Mischief.Render.Shader.Bindings where

import Data.Data
import Data.Default (Default (def))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), U1 (U1), V1, (:*:) ((:*:)))
import GHC.TypeLits

data BindingType = BSampler | BTexture2d deriving (Show)

data Binding (index :: Nat) a = EmptyBinding | Binding a

instance Default (Binding n a) where
  def = EmptyBinding

data BindingData = BindingData
  { bType :: BindingType,
    index :: Integer
  }
  deriving (Show)

newtype Bindings = Bindings [BindingData] deriving newtype (Semigroup, Show)

class (Default a) => Bindable a where
  collectBindings :: Proxy a -> Bindings
  default collectBindings :: (Generic a, Bindable' (Rep a)) => Proxy a -> Bindings
  collectBindings _ = collectBindings' (Proxy @(Rep a))

class Bindable' f where
  collectBindings' :: Proxy f -> Bindings

instance Bindable' V1 where
  collectBindings' _ = Bindings []

instance Bindable' U1 where
  collectBindings' _ = Bindings []

instance (Bindable' f, Bindable' g) => Bindable' (f :*: g) where
  collectBindings' _ = collectBindings' (Proxy @f) <> collectBindings' (Proxy @g)

instance (Bindable c) => Bindable' (K1 i c) where
  collectBindings' _ = collectBindings (Proxy @c)

instance (Bindable' f) => Bindable' (M1 i t f) where
  collectBindings' _ = collectBindings' (Proxy @f)

data Sampler = Sampler

instance (KnownNat n) => Bindable (Binding n Sampler) where
  collectBindings _ = Bindings [BindingData {bType = BSampler, index = natVal (Proxy @n)}]

data Texture2d = Texture2d

instance (KnownNat n) => Bindable (Binding n Texture2d) where
  collectBindings _ = Bindings [BindingData {bType = BTexture2d, index = natVal (Proxy @n)}]

data Test' = Test'
  { sampler :: Binding 0 Sampler,
    texture :: Binding 1 Texture2d
  }
  deriving (Generic, Default, Bindable)

test' = Test' (Binding Sampler) (Binding Texture2d)

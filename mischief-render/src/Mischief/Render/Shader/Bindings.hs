module Mischief.Render.Shader.Bindings where

import Data.Data
import Data.Default (Default (def))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), U1 (U1), V1, (:*:) ((:*:)))
import GHC.TypeLits
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types

data Binding (index :: Nat) a where
  Binding :: forall b index. Expr b -> Binding index (Expr b)

instance (KnownNat n) => Default (Binding n (Expr a)) where
  def = Binding $ BindingVar (natVal (Proxy @n))

data BindingData = BindingData
  { bType :: Text,
    index :: Integer
  }
  deriving (Show)

newtype Bindings = Bindings [BindingData] deriving newtype (Semigroup, Show)

class (Default a) => Bindable a where
  collectBindings :: Proxy a -> Bindings
  default collectBindings :: (Bindable' (Rep a)) => Proxy a -> Bindings
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

data Sampler' = Sampler'

type Sampler = Expr (Custom Sampler')

instance (KnownNat n) => Bindable (Binding n Sampler) where
  collectBindings :: Proxy (Binding n Sampler) -> Bindings
  collectBindings _ = Bindings [BindingData {bType = T.pack "sampler", index = natVal (Proxy @n)}]

data Texture2d' = Texture2d'

type Texture2d = Expr (Custom Texture2d')

instance (KnownNat n) => Bindable (Binding n Texture2d) where
  collectBindings _ = Bindings [BindingData {bType = T.pack "texture_2d<f32>", index = natVal (Proxy @n)}]

data Test' = Test'
  { sampler :: Binding 0 Sampler,
    texture :: Binding 1 Texture2d
  }
  deriving (Generic, Default, Bindable)

instance Bindable ()

instance (KnownNat n, ReflType (Primitive a)) => Bindable (Binding n (Expr (Primitive a))) where
  collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (Primitive a)), index = natVal (Proxy @n)}]

instance (KnownNat n, ReflType (VectorType m a)) => Bindable (Binding n (Expr (VectorType m a))) where
  collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (VectorType m a)), index = natVal (Proxy @n)}]

instance (KnownNat n, ReflType (ArrayType m a)) => Bindable (Binding n (Expr (ArrayType m a))) where
  collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (ArrayType m a)), index = natVal (Proxy @n)}]

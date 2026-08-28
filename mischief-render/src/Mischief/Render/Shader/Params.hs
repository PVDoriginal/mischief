{-# LANGUAGE OverloadedStrings #-}

module Mischief.Render.Shader.Params where

import Data.Data
import Data.Default
import Data.Singletons (SingI)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.Records
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import Mischief.Render.Shader.Bindings
import Mischief.Render.Shader.Singletons
import Mischief.Render.Shader.State

data BuiltIn (name :: Symbol) a where
  BuiltIn :: forall a name. (Expr a) -> BuiltIn name (Expr a)

instance (SingI a, KnownSymbol name) => Default (BuiltIn name (Expr a)) where
  def = BuiltIn $ BuiltInVar (T.pack $ symbolVal (Proxy @name))

data Location (n :: Nat) a where
  Location :: forall a n. (Expr a) -> Location n (Expr a)

instance (SingI a, KnownNat n) => Default (Location n (Expr a)) where
  def = Location $ LocationVar $ fromInteger (natVal (Proxy @n))

data ParamKind = LocParam Integer | BuiltInParam Text deriving (Show)

data ParamData = ParamData
  { pType :: Text,
    index :: ParamKind
  }
  deriving (Show)

newtype Params = Params [ParamData] deriving newtype (Semigroup, Show)

data Value where
  Value :: Expr a -> Value

newtype Values = Values [Value] deriving newtype (Semigroup)

class (Typeable a, Default a) => ShaderParam a where
  collectParams :: Proxy a -> Params
  default collectParams :: (ShaderParam' (Rep a)) => Proxy a -> Params
  collectParams _ = collectParams' (Proxy @(Rep a))

  getName :: Proxy a -> Text
  getName = T.pack . show . typeRep

  getType :: Proxy a -> Text -> Text
  getType a "" = getName a
  getType a n = n <> " : " <> getName a

  collectValues :: a -> Values
  default collectValues :: (Generic a, ShaderParam' (Rep a)) => a -> Values
  collectValues a = collectValues' (from a)

-- collectValues :: a -> Values
-- defualt CollectValues :: ()

class ShaderParam' f where
  collectParams' :: Proxy f -> Params
  collectValues' :: f p -> Values

instance ShaderParam' V1 where
  collectParams' _ = Params []
  collectValues' _ = Values []

instance ShaderParam' U1 where
  collectParams' _ = Params []
  collectValues' _ = Values []

instance (ShaderParam' f, ShaderParam' g) => ShaderParam' (f :*: g) where
  collectParams' _ = collectParams' (Proxy @f) <> collectParams' (Proxy @g)
  collectValues' (f :*: g) = collectValues' f <> collectValues' g

instance (ShaderParam c) => ShaderParam' (K1 i c) where
  collectParams' _ = collectParams (Proxy @c)
  collectValues' (K1 x) = collectValues x

instance (ShaderParam' f) => ShaderParam' (M1 i t f) where
  collectParams' _ = collectParams' (Proxy @f)
  collectValues' (M1 x) = collectValues' x

instance ShaderParam ()

instance (Typeable a, KnownSymbol s, SingI (Primitive a)) => ShaderParam (BuiltIn s (Expr (Primitive a))) where
  collectParams _ = Params [ParamData {pType = typeToWGSL (undefined :: Expr (Primitive a)), index = BuiltInParam $ T.pack $ symbolVal (Proxy @s)}]
  collectValues (BuiltIn a) = Values [Value a]

  getName _ = ""
  getType _ "" = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> typeToWGSL (undefined :: Expr (Primitive a))
  getType _ n = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (Primitive a))

instance (Typeable a, Typeable m, KnownSymbol s, SingI (VectorType m a)) => ShaderParam (BuiltIn s (Expr (VectorType m a))) where
  collectParams _ = Params [ParamData {pType = typeToWGSL (undefined :: Expr (VectorType m a)), index = BuiltInParam $ T.pack $ symbolVal (Proxy @s)}]
  collectValues (BuiltIn a) = Values [Value a]
  getName _ = ""
  getType _ "" = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> typeToWGSL (undefined :: Expr (VectorType m a))
  getType _ n = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (VectorType m a))

instance (Typeable a, Typeable m, KnownSymbol s, SingI (ArrayType m a)) => ShaderParam (BuiltIn s (Expr (ArrayType m a))) where
  collectParams _ = Params [ParamData {pType = typeToWGSL (undefined :: Expr (ArrayType m a)), index = BuiltInParam $ T.pack $ symbolVal (Proxy @s)}]
  collectValues (BuiltIn a) = Values [Value a]
  getName _ = ""
  getType _ "" = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> typeToWGSL (undefined :: Expr (ArrayType m a))
  getType _ n = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (ArrayType m a))

instance (Typeable a, KnownNat n, SingI (Primitive a)) => ShaderParam (Location n (Expr (Primitive a))) where
  collectParams _ = Params [ParamData {pType = typeToWGSL (undefined :: Expr (Primitive a)), index = LocParam $ fromInteger $ natVal (Proxy @n)}]
  collectValues (Location a) = Values [Value a]
  getName _ = ""
  getType _ "" = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> typeToWGSL (undefined :: Expr (Primitive a))
  getType _ n = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (Primitive a))

instance (Typeable m, Typeable a, KnownNat n, SingI (VectorType m a)) => ShaderParam (Location n (Expr (VectorType m a))) where
  collectParams _ = Params [ParamData {pType = typeToWGSL (undefined :: Expr (VectorType m a)), index = LocParam $ fromInteger $ natVal (Proxy @n)}]
  collectValues (Location a) = Values [Value a]
  getName _ = ""
  getType _ "" = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> typeToWGSL (undefined :: Expr (VectorType m a))
  getType _ n = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (VectorType m a))

instance (Typeable m, Typeable a, KnownNat n, SingI (ArrayType m a)) => ShaderParam (Location n (Expr (ArrayType m a))) where
  collectParams _ = Params [ParamData {pType = typeToWGSL (undefined :: Expr (ArrayType m a)), index = LocParam $ fromInteger $ natVal (Proxy @n)}]
  collectValues (Location a) = Values [Value a]
  getName _ = ""
  getType _ "" = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> typeToWGSL (undefined :: Expr (ArrayType m a))
  getType _ n = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (ArrayType m a))

class ReadParam a b | a -> b where
  get :: a -> b

instance ReadParam (BuiltIn s (Expr a)) (Expr a) where
  get (BuiltIn a) = a

instance ReadParam (Location n (Expr a)) (Expr a) where
  get (Location a) = a

instance (AssociatedExpr a ~ b) => ReadParam (Binding n a) (Expr b) where
  get (Binding a) = a
  get _ = undefined

newtype Struct a = Struct (VarIndex, a)

newtype StructField a = StructField (VarIndex, a)

instance (HasField a b c) => HasField a (Struct b) (StructField c) where
  getField (Struct (i, a)) = StructField (i, getField @a a)

class SetParam a b | b -> a where
  set :: a -> b

instance SetParam (Expr a) (BuiltIn s (Expr a)) where
  set = BuiltIn

instance SetParam (Expr a) (Location n (Expr a)) where
  set = Location
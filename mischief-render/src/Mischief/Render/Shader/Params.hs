{-# LANGUAGE AllowAmbiguousTypes #-}
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

data BuiltInParam (n :: Symbol) b = BuiltInParam

data LocationParam (n :: Nat) b = LocationParam

type family BuiltIn a n b where
  BuiltIn Internal n b = BuiltInParam n b
  BuiltIn GPU n (Expr b) = Expr b

type family Location a n b where
  Location Internal n b = LocationParam n b
  Location GPU n (Expr b) = Expr b

-- data BuiltIn (name :: Symbol) a where
--   BuiltIn :: forall a name. (Expr a) -> BuiltIn name (Expr a)

-- data Location (n :: Nat) a where
--   Location :: forall a n. (Expr a) -> Location n (Expr a)

data ParamKind = LocKind Integer | BuiltInKind Text deriving (Show)

data ParamData = ParamData
  { pType :: Text,
    index :: ParamKind
  }
  deriving (Show)

newtype Params = Params [ParamData] deriving newtype (Semigroup, Show)

data Value where
  Value :: Expr a -> Value

newtype Values = Values [Value] deriving newtype (Semigroup)

class (Typeable (a GPU)) => ShaderParam a where
  collectParams :: Params
  default collectParams :: (GCollectParams (Rep (a Internal))) => Params
  collectParams = gCollectParams @(Rep (a Internal))

  getName :: Text
  getName = T.map (\case ' ' -> '_'; n -> n) $ T.pack $ show (typeRep (Proxy @(a GPU)))

  getType :: Text -> Text
  getType "" = getName @a
  getType n = n <> " : " <> getName @a

  -- collectValues :: a -> Values
  -- default collectValues :: (Generic a, ShaderParam' (Rep (a ))) => a -> Values
  -- collectValues a = collectValues' (from a)

  dummyParam :: a GPU
  default dummyParam :: (Generic (a GPU), GDummyParam (Rep (a Internal)) (Rep (a GPU))) => a GPU
  dummyParam = to (gDummyParam @(Rep (a Internal)) @(Rep (a GPU)))

  collectValues :: a GPU -> Values
  default collectValues :: (Generic (a GPU), GCollectValues (Rep (a GPU))) => a GPU -> Values
  collectValues a = gCollectValues (from a)

class CollectParam a where
  collectParam :: ParamData
  getName' :: Text
  getType' :: Text -> Text

class GCollectParams f where
  gCollectParams :: Params

instance GCollectParams V1 where
  gCollectParams = undefined

instance GCollectParams U1 where
  gCollectParams = Params []

instance (GCollectParams f, GCollectParams g) => GCollectParams (f :*: g) where
  gCollectParams = gCollectParams @f <> gCollectParams @g

instance (CollectParam c) => GCollectParams (K1 i c) where
  gCollectParams = Params [collectParam @c]

instance (GCollectParams f) => GCollectParams (M1 i t f) where
  gCollectParams = gCollectParams @f

instance (KnownSymbol s, SingI (Primitive a)) => CollectParam (BuiltInParam s (Expr (Primitive a))) where
  collectParam = ParamData {pType = typeToWGSL (undefined :: Expr (Primitive a)), index = BuiltInKind $ T.pack $ symbolVal (Proxy @s)}
  getName' = ""

  getType' "" = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> typeToWGSL (undefined :: Expr (Primitive a))
  getType' n = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (Primitive a))

instance (KnownSymbol s, SingI (VectorType m a)) => CollectParam (BuiltInParam s (Expr (VectorType m a))) where
  collectParam = ParamData {pType = typeToWGSL (undefined :: Expr (VectorType m a)), index = BuiltInKind $ T.pack $ symbolVal (Proxy @s)}
  getName' = ""
  getType' "" = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> typeToWGSL (undefined :: Expr (VectorType m a))
  getType' n = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (VectorType m a))

instance (KnownSymbol s, SingI (ArrayType m a)) => CollectParam (BuiltInParam s (Expr (ArrayType m a))) where
  collectParam = ParamData {pType = typeToWGSL (undefined :: Expr (ArrayType m a)), index = BuiltInKind $ T.pack $ symbolVal (Proxy @s)}
  getName' = ""
  getType' "" = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> typeToWGSL (undefined :: Expr (ArrayType m a))
  getType' n = "@builtin(" <> T.pack (symbolVal (Proxy @s)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (ArrayType m a))

instance (KnownNat n, SingI (Primitive a)) => CollectParam (LocationParam n (Expr (Primitive a))) where
  collectParam = ParamData {pType = typeToWGSL (undefined :: Expr (Primitive a)), index = LocKind $ fromInteger $ natVal (Proxy @n)}
  getName' = ""
  getType' "" = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> typeToWGSL (undefined :: Expr (Primitive a))
  getType' n = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (Primitive a))

instance (KnownNat n, SingI (VectorType m a)) => CollectParam (LocationParam n (Expr (VectorType m a))) where
  collectParam = ParamData {pType = typeToWGSL (undefined :: Expr (VectorType m a)), index = LocKind $ fromInteger $ natVal (Proxy @n)}
  getName' = ""
  getType' "" = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> typeToWGSL (undefined :: Expr (VectorType m a))
  getType' n = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (VectorType m a))

instance (KnownNat n, SingI (ArrayType m a)) => CollectParam (LocationParam n (Expr (ArrayType m a))) where
  collectParam = ParamData {pType = typeToWGSL (undefined :: Expr (ArrayType m a)), index = LocKind $ fromInteger $ natVal (Proxy @n)}
  getName' = ""
  getType' "" = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> typeToWGSL (undefined :: Expr (ArrayType m a))
  getType' n = "@location(" <> T.pack (show $ natVal (Proxy @n)) <> ") " <> n <> ": " <> typeToWGSL (undefined :: Expr (ArrayType m a))

class DummyParam a b where
  dummyParam' :: b

class GDummyParam f g where
  gDummyParam :: g p

instance GDummyParam V1 V1 where
  gDummyParam = undefined

instance GDummyParam U1 U1 where
  gDummyParam = U1

instance (GDummyParam f1 g1, GDummyParam f2 g2) => GDummyParam (f1 :*: f2) (g1 :*: g2) where
  gDummyParam = gDummyParam @f1 @g1 :*: gDummyParam @f2 @g2

instance (DummyParam a b) => GDummyParam (K1 i a) (K1 i b) where
  gDummyParam = K1 $ dummyParam' @a @b

instance (GDummyParam a b) => GDummyParam (M1 i t a) (M1 i' t' b) where
  gDummyParam = M1 $ gDummyParam @a @b

instance (KnownSymbol n, SingI b) => DummyParam (BuiltInParam n a) (Expr b) where
  dummyParam' = BuiltInVar (T.pack $ symbolVal $ Proxy @n)

instance (KnownNat n, SingI b) => DummyParam (LocationParam n a) (Expr b) where
  dummyParam' = LocationVar (fromInteger $ natVal $ Proxy @n)

class GCollectValues f where
  gCollectValues :: f p -> Values

instance GCollectValues V1 where
  gCollectValues _ = undefined

instance GCollectValues U1 where
  gCollectValues _ = Values []

instance (GCollectValues f, GCollectValues g) => GCollectValues (f :*: g) where
  gCollectValues (f :*: g) = gCollectValues f <> gCollectValues g

instance GCollectValues (K1 i (Expr b)) where
  gCollectValues (K1 c) = Values [Value c]

instance (GCollectValues f) => GCollectValues (M1 i t f) where
  gCollectValues (M1 f) = gCollectValues f

-- instance (KnownNat n, Typeable (Location' n (Expr a) GPU)) => ShaderParam (Location' n (Expr a)) where
--   collectParams = undefined
--   getName = undefined
--   getType = undefined
--   dummyParam = undefined
--   collectValues a = Values [a]

newtype LocInternal n a f = Loc (Location f n a) deriving stock (Generic)

instance
  ( Typeable a,
    Location GPU n a ~ Expr b,
    KnownNat n,
    CollectParam (LocationParam n a),
    DummyParam
      (LocationParam n a)
      (Location GPU n a)
  ) =>
  ShaderParam (LocInternal n a)

type Loc n a = LocInternal n a GPU

newtype BInInternal n a f = BIn (BuiltIn f n a) deriving stock (Generic)

instance
  ( Typeable a,
    BuiltIn GPU n a ~ Expr b,
    KnownSymbol n,
    CollectParam (BuiltInParam n a),
    DummyParam
      (BuiltInParam n a)
      (BuiltIn GPU n a)
  ) =>
  ShaderParam (BInInternal n a)

type BIn n a = BInInternal n a GPU
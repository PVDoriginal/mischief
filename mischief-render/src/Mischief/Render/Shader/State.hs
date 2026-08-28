{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Mischief.Render.Shader.State where

import Control.Monad.State
import Data.Data hiding (cast)
import Data.Default
import Data.Singletons (Sing, SingI, SingKind (Demote), demote)
import Data.Singletons.Base.CustomStar (genSingletons)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Internal.Check
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal)
import Mischief.ECS.Log (text)
import Mischief.Render.Shader.Singletons
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (cos, sin)

data ShaderState = ShaderState
  { counter :: VarIndex,
    ast :: Stmt
  }

newtype VarIndex = VarIndex Integer
  deriving newtype (Show, Num)

nameIndex :: VarIndex -> Text
nameIndex i = "v" <> T.pack (show i)

data Stmt where
  Concat :: Stmt -> Stmt -> Stmt
  Let :: VarIndex -> Expr a -> Stmt
  Return :: Expr a -> Stmt
  Empty :: Stmt

data CustomType a = CustomType

instance Eq (CustomType a) where
  _ == _ = False

data Expr (a :: Types) where
  Function :: Text -> [Param] -> Expr a
  Add :: (SingI a) => Expr a -> Expr a -> Expr a
  Sub :: (SingI a) => Expr a -> Expr a -> Expr a
  Mult :: (SingI a) => Expr a -> Expr a -> Expr a
  Div :: (SingI a) => Expr a -> Expr a -> Expr a
  Neg :: (SingI a) => Expr a -> Expr a
  Abs :: (SingI a) => Expr a -> Expr a
  Signum :: (SingI a) => Expr a -> Expr a
  Cast :: (SingI a, SingI b) => Proxy b -> Expr a -> Expr b
  CastVec :: (SingI (VectorType n a), SingI (VectorType n b)) => Proxy (VectorType n b) -> Expr (VectorType n a) -> Expr (VectorType n b)
  ConstantInt :: Int -> Expr (Primitive TInt)
  ConstantFloat :: Float -> Expr (Primitive TFloat)
  ConstantUInt :: Int -> Expr (Primitive TUInt)
  ConstantArray :: forall (n :: Nat) (a' :: Types). (PrintArrayElements n a', SingI (ArrayType n a')) => Proxy (ArrayType n a') -> ArrayInit n a' -> Expr (ArrayType n a')
  ConstantVec :: forall (n :: VecLength) (a' :: PrimitiveTypes). (PrintVecElements n a', SingI (VectorType n a')) => Proxy (VectorType n a') -> VecInit n a' -> Expr (VectorType n a')
  AccessField :: (SingI a) => Expr a -> Text -> Expr b
  Index :: (SingI (ArrayType n a)) => Expr (ArrayType n a) -> Expr (Primitive TUInt) -> Expr a
  Var :: (SingI a) => VarIndex -> Expr a
  BindingVar :: (SingI a) => Integer -> Expr a
  BuiltInVar :: (SingI a) => Text -> Expr a
  StructInit :: Text -> [Text] -> Expr a
  LocationVar :: (SingI a) => Integer -> Expr a
  Void :: (SingI a) => Expr a

data Param where
  Param :: (SingI a) => Expr a -> Param

type family IsAlgebric (a :: Types) where
  IsAlgebric (Primitive a) = True
  IsAlgebric (VectorType n a) = True
  IsAlgebric a = False

type family IsIntegral (a :: Types) where
  IsIntegral (Primitive TInt) = True
  IsIntegral (Primitive TUInt) = True
  IsIntegral (VectorType n TInt) = True
  IsIntegral (VectorType n TUInt) = True
  IsIntegral a = False

type family VectorOrSingleOf a where
  VectorOrSingleOf (VectorType n a) = a
  VectorOrSingleOf (Primitive a) = a

instance (SingI a) => Num (Expr (Primitive a)) where
  (+) = Add
  (*) = Mult
  abs = Abs
  signum = Signum
  fromInteger i =
    let t = demote @a
     in case t of
          TInt -> unsafeCoerce $ ConstantInt (fromInteger i)
          TFloat -> unsafeCoerce $ ConstantFloat (fromInteger i)
          TUInt -> unsafeCoerce $ ConstantUInt (fromInteger i)
  negate = Neg

instance (SingI (VectorType n a)) => Num (Expr (VectorType n a)) where
  (+) = Add
  (*) = Mult
  abs = Abs
  signum = Signum
  fromInteger i =
    let t = demote @(VectorType n a)
     in case t of
          VectorType VL2 TFloat -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL2 TFloat)) (fromInteger i, fromInteger i)
          VectorType VL2 TInt -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL2 TInt)) (fromInteger i, fromInteger i)
          VectorType VL2 TUInt -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL2 TUInt)) (fromInteger i, fromInteger i)
          VectorType VL3 TFloat -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL3 TFloat)) (fromInteger i, fromInteger i, fromInteger i)
          VectorType VL3 TInt -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL3 TInt)) (fromInteger i, fromInteger i, fromInteger i)
          VectorType VL3 TUInt -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL3 TUInt)) (fromInteger i, fromInteger i, fromInteger i)
          VectorType VL4 TFloat -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL4 TFloat)) (fromInteger i, fromInteger i, fromInteger i, fromInteger i)
          VectorType VL4 TInt -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL4 TInt)) (fromInteger i, fromInteger i, fromInteger i, fromInteger i)
          VectorType VL4 TUInt -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL4 TUInt)) (fromInteger i, fromInteger i, fromInteger i, fromInteger i)
          _ -> undefined
  negate = Neg

instance Fractional (Expr (Primitive TFloat)) where
  fromRational = ConstantFloat . fromRational
  recip = Div 1
  (/) = Div

instance (SingI n) => Fractional (Expr (VectorType n TFloat)) where
  fromRational i =
    let t = demote @n
     in case t of
          VL2 -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL2 TFloat)) (fromRational i, fromRational i)
          VL3 -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL3 TFloat)) (fromRational i, fromRational i, fromRational i)
          VL4 -> unsafeCoerce $ ConstantVec (Proxy @(VectorType VL4 TFloat)) (fromRational i, fromRational i, fromRational i, fromRational i)
  recip = Div 1
  (/) = Div

type family VecInit (n :: VecLength) (a :: PrimitiveTypes) where
  VecInit VL2 a = (Expr (Primitive a), Expr (Primitive a))
  VecInit VL3 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))
  VecInit VL4 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))

class PrintVecElements (n :: VecLength) (a :: PrimitiveTypes) where
  printVecElements :: VecInit n a -> Text

instance (SingI a) => PrintVecElements VL2 a where
  printVecElements (a, b) = exprToWGSL a <> ", " <> exprToWGSL b

instance (SingI a) => PrintVecElements VL3 a where
  printVecElements (a, b, c) = exprToWGSL a <> ", " <> exprToWGSL b <> ", " <> exprToWGSL c

instance (SingI a) => PrintVecElements VL4 a where
  printVecElements (a, b, c, d) = exprToWGSL a <> ", " <> exprToWGSL b <> ", " <> exprToWGSL c <> ", " <> exprToWGSL d

type family ArrayInit (n :: Nat) (a :: Types) where
  ArrayInit 2 a = (Expr a, Expr a)
  ArrayInit 3 a = (Expr a, Expr a, Expr a)
  ArrayInit 4 a = (Expr a, Expr a, Expr a, Expr a)

class PrintArrayElements (n :: Nat) (a :: Types) where
  printArrayElements :: ArrayInit n a -> Text

instance (SingI a) => PrintArrayElements 2 a where
  printArrayElements (a, b) = exprToWGSL a <> ", " <> exprToWGSL b

instance (SingI a) => PrintArrayElements 3 a where
  printArrayElements (a, b, c) = exprToWGSL a <> ", " <> exprToWGSL b <> ", " <> exprToWGSL c

showArgs :: [Param] -> Text
showArgs [] = ""
showArgs [Param x] = exprToWGSL x
showArgs ((Param x) : xs) = exprToWGSL x <> ", " <> showArgs xs

exprToWGSL :: Expr a -> Text
exprToWGSL (Function t x) = t <> "(" <> showArgs x <> ")"
exprToWGSL (Add a b) = "(" <> exprToWGSL a <> " + " <> exprToWGSL b <> ")"
exprToWGSL (Mult a b) = "(" <> exprToWGSL a <> " * " <> exprToWGSL b <> ")"
exprToWGSL (Sub a b) = "(" <> exprToWGSL a <> " - " <> exprToWGSL b <> ")"
exprToWGSL (Div a b) = "(" <> exprToWGSL a <> " / " <> exprToWGSL b <> ")"
exprToWGSL (Neg a) = "(-" <> exprToWGSL a <> ")"
exprToWGSL (Abs a) = exprToWGSL (Function "abs" [Param a])
exprToWGSL (Signum a) = exprToWGSL (Function "sign" [Param a])
exprToWGSL (Cast (_ :: Proxy b) (a :: Expr a)) =
  let aType = demote @a
      bType = demote @b
   in if aType == bType
        then exprToWGSL a
        else case bType of
          Primitive TInt -> "i32(" <> exprToWGSL a <> ")"
          Primitive TFloat -> "f32(" <> exprToWGSL a <> ")"
          Primitive TUInt -> "u32(" <> exprToWGSL a <> ")"
          _ -> undefined
exprToWGSL (CastVec (_ :: Proxy b) (a :: Expr a)) =
  let aType = demote @a
      bType = demote @b
   in if aType == bType
        then exprToWGSL a
        else case bType of
          VectorType VL2 TInt -> "vec2<i32>(" <> exprToWGSL a <> ")"
          VectorType VL2 TFloat -> "vec2<f32>(" <> exprToWGSL a <> ")"
          VectorType VL2 TUInt -> "vec2<u32>(" <> exprToWGSL a <> ")"
          VectorType VL3 TInt -> "vec3<i32>(" <> exprToWGSL a <> ")"
          VectorType VL3 TFloat -> "vec3<f32>(" <> exprToWGSL a <> ")"
          VectorType VL3 TUInt -> "vec3<u32>(" <> exprToWGSL a <> ")"
          VectorType VL4 TInt -> "vec4<i32>(" <> exprToWGSL a <> ")"
          VectorType VL4 TFloat -> "vec4<f32>(" <> exprToWGSL a <> ")"
          VectorType VL4 TUInt -> "vec4<u32>(" <> exprToWGSL a <> ")"
          _ -> undefined
exprToWGSL (ConstantArray @n @a' (_ :: Proxy a) x) =
  let elements = printArrayElements @n @a' x
   in case demote @a of
        ArrayType n t -> "array<" <> typeToWGSL' t <> ", " <> T.pack (show n) <> ">(" <> elements <> ")"
        _ -> undefined
exprToWGSL (ConstantVec @n @a' (_ :: Proxy a) x) =
  let elements = printVecElements @n @a' x
   in case demote @a of
        VectorType VL2 TInt -> "vec2<i32>(" <> elements <> ")"
        VectorType VL2 TFloat -> "vec2<f32>(" <> elements <> ")"
        VectorType VL2 TUInt -> "vec2<u32>(" <> elements <> ")"
        VectorType VL3 TInt -> "vec3<i32>(" <> elements <> ")"
        VectorType VL3 TFloat -> "vec3<f32>(" <> elements <> ")"
        VectorType VL3 TUInt -> "vec3<u32>(" <> elements <> ")"
        VectorType VL4 TInt -> "vec4<i32>(" <> elements <> ")"
        VectorType VL4 TFloat -> "vec4<f32>(" <> elements <> ")"
        VectorType VL4 TUInt -> "vec4<u32>(" <> elements <> ")"
        _ -> undefined
exprToWGSL (ConstantInt x) = T.pack $ show x <> "i"
exprToWGSL (ConstantFloat x) = T.pack $ show x <> "f"
exprToWGSL (ConstantUInt x) = T.pack $ show x <> "u"
exprToWGSL (Var x) = nameIndex x
exprToWGSL (BindingVar x) = "b" <> T.pack (show x)
exprToWGSL Void = ""
exprToWGSL (AccessField x f) = exprToWGSL x <> "." <> f
exprToWGSL (Index a i) = exprToWGSL a <> "[" <> exprToWGSL i <> "]"
exprToWGSL (BuiltInVar x) = "input." <> x
exprToWGSL (LocationVar x) = "input.l" <> T.pack (show x)
exprToWGSL (StructInit "" [p]) = p
exprToWGSL (StructInit name params) = name <> "(" <> T.intercalate "," params <> ")"

stmtToWGSL :: Stmt -> Text
stmtToWGSL Empty = ""
stmtToWGSL (Concat Empty a) = stmtToWGSL a
stmtToWGSL (Concat a b) = stmtToWGSL a <> "\n" <> stmtToWGSL b
stmtToWGSL (Let a b) = "  let " <> nameIndex a <> " = " <> exprToWGSL b <> ";"
stmtToWGSL (Return a) = "  return " <> exprToWGSL a <> ";"

newtype Shader a = Shader (State ShaderState a) deriving newtype (Functor, Applicative, Monad, MonadState ShaderState)

typeToWGSL :: forall (a :: Types). (SingI a) => Expr a -> Text
typeToWGSL _ = typeToWGSL' (demote @a)

typeToWGSL' :: Types -> Text
typeToWGSL' a = case a of
  Primitive TInt -> "i32"
  Primitive TFloat -> "f32"
  Primitive TUInt -> "u32"
  ArrayType n t -> "array<" <> typeToWGSL' t <> ", " <> T.pack (show n) <> ">"
  VectorType VL2 TFloat -> "vec2<f32>"
  VectorType VL2 TInt -> "vec2<i32>"
  VectorType VL2 TUInt -> "vec2<u32>"
  VectorType VL3 TFloat -> "vec3<f32>"
  VectorType VL3 TInt -> "vec3<i32>"
  VectorType VL3 TUInt -> "vec3<u32>"
  VectorType VL4 TFloat -> "vec4<f32>"
  VectorType VL4 TInt -> "vec4<i32>"
  VectorType VL4 TUInt -> "vec4<u32>"
  _ -> undefined

addStmt :: Stmt -> Shader ()
addStmt stmt = do
  state <- get
  put state {ast = Concat state.ast stmt}

newVarIndex :: Shader VarIndex
newVarIndex = do
  ShaderState {counter, ast} <- get
  put (ShaderState {counter = counter + 1, ast})
  pure counter

var :: (SingI a) => Expr a -> Shader (Expr a)
var expr = do
  index <- newVarIndex
  addStmt (Let index expr)
  pure $ Var index

at :: forall n a. (SingI (ArrayType n a)) => Expr (ArrayType n a) -> Expr (Primitive TUInt) -> Expr a
at = Index

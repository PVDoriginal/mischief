{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}

module Mischief.Render.Shader.State where

import Control.Monad.State
import Data.Data hiding (cast)
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Internal.Check
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal)
import Mischief.ECS.Log (text)
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

data PrimitiveTypes = TInt | TFloat | TUInt deriving (Eq)

data VecLength = VL2 | VL3 | VL4 deriving (Eq)

data Types where
  Primitive :: PrimitiveTypes -> Types
  ArrayType :: Nat -> Types -> Types
  VectorType :: VecLength -> PrimitiveTypes -> Types
  Custom :: a -> Types

instance Eq Types where
  Primitive a == Primitive b = a == b
  (ArrayType n a) == (ArrayType m b) = n == m && a == b
  (VectorType vl1 a) == (VectorType vl2 b) = vl1 == vl2 && a == b
  _ == _ = False

data CustomType a = CustomType

instance Eq (CustomType a) where
  _ == _ = False

data Expr (a :: Types) where
  Function :: Text -> [Param] -> Expr a
  Add :: Expr a -> Expr a -> Expr a
  Sub :: Expr a -> Expr a -> Expr a
  Mult :: Expr a -> Expr a -> Expr a
  Div :: Expr a -> Expr a -> Expr a
  Neg :: Expr a -> Expr a
  Abs :: Expr a -> Expr a
  Signum :: Expr a -> Expr a
  Cast :: (ReflType a, ReflType b) => Proxy b -> Expr a -> Expr b
  CastVec :: (ReflType (VectorType n b), ReflType (VectorType n a)) => Proxy (VectorType n b) -> Expr (VectorType n a) -> Expr (VectorType n b)
  ConstantInt :: Int -> Expr (Primitive TInt)
  ConstantFloat :: Float -> Expr (Primitive TFloat)
  ConstantUInt :: Int -> Expr (Primitive TUInt)
  ConstantArray :: forall (n :: Nat) (a' :: Types). (PrintArrayElements n a', ReflType (ArrayType n a')) => Proxy (ArrayType n a') -> ArrayInit n a' -> Expr (ArrayType n a')
  ConstantVec :: forall (n :: VecLength) (a' :: PrimitiveTypes). (PrintVecElements n a', ReflType (VectorType n a')) => Proxy (VectorType n a') -> VecInit n a' -> Expr (VectorType n a')
  AccessField :: Expr a -> Text -> Expr b
  Index :: Expr (ArrayType n a) -> Expr (Primitive TUInt) -> Expr a
  Var :: VarIndex -> Expr a
  BindingVar :: Integer -> Expr a
  BuiltInVar :: Text -> Expr a
  StructInit :: Text -> [Text] -> Expr a
  LocationVar :: Integer -> Expr a
  Void :: Expr a

data Param where
  Param :: Expr a -> Param

type family IsAlgebric (a :: Types) where
  IsAlgebric (Primitive a) = True
  IsAlgebric (VectorType n a) = True
  IsAlgebric a = False

instance (ReflType (Primitive a)) => Num (Expr (Primitive a)) where
  (+) = Add
  (*) = Mult
  abs = Abs
  signum = Signum
  fromInteger i =
    let t = reflType (undefined :: Expr (Primitive a))
     in case t of
          Primitive TInt -> unsafeCoerce $ ConstantInt (fromInteger i)
          Primitive TFloat -> unsafeCoerce $ ConstantFloat (fromInteger i)
          Primitive TUInt -> unsafeCoerce $ ConstantUInt (fromInteger i)
          _ -> undefined
  negate = Neg

instance (ReflType (VectorType n a)) => Num (Expr (VectorType n a)) where
  (+) = Add
  (*) = Mult
  abs = Abs
  signum = Signum
  fromInteger i =
    let t = reflType (undefined :: Expr (VectorType n a))
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

instance (ReflType (Primitive a)) => Enum (Expr (Primitive a)) where
  toEnum i = fromInteger (toInteger i)
  fromEnum (ConstantInt x) = x
  fromEnum _ = undefined

type family VecInit (n :: VecLength) (a :: PrimitiveTypes) where
  VecInit VL2 a = (Expr (Primitive a), Expr (Primitive a))
  VecInit VL3 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))
  VecInit VL4 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))

class PrintVecElements (n :: VecLength) (a :: PrimitiveTypes) where
  printVecElements :: VecInit n a -> Text

instance PrintVecElements VL2 a where
  printVecElements (a, b) = exprToWGSL a <> ", " <> exprToWGSL b

instance PrintVecElements VL3 a where
  printVecElements (a, b, c) = exprToWGSL a <> ", " <> exprToWGSL b <> ", " <> exprToWGSL c

instance PrintVecElements VL4 a where
  printVecElements (a, b, c, d) = exprToWGSL a <> ", " <> exprToWGSL b <> ", " <> exprToWGSL c <> ", " <> exprToWGSL d

type family ArrayInit (n :: Nat) (a :: Types) where
  ArrayInit 2 a = (Expr a, Expr a)
  ArrayInit 3 a = (Expr a, Expr a, Expr a)
  ArrayInit 4 a = (Expr a, Expr a, Expr a, Expr a)

class PrintArrayElements (n :: Nat) (a :: Types) where
  printArrayElements :: ArrayInit n a -> Text

instance PrintArrayElements 2 a where
  printArrayElements (a, b) = exprToWGSL a <> ", " <> exprToWGSL b

instance PrintArrayElements 3 a where
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
  let aType = reflType (undefined :: Expr a)
      bType = reflType (undefined :: Expr b)
   in if aType == bType
        then exprToWGSL a
        else case bType of
          Primitive TInt -> "i32(" <> exprToWGSL a <> ")"
          Primitive TFloat -> "f32(" <> exprToWGSL a <> ")"
          Primitive TUInt -> "u32(" <> exprToWGSL a <> ")"
          _ -> undefined
exprToWGSL (CastVec (_ :: Proxy b) (a :: Expr a)) =
  let aType = reflType (undefined :: Expr a)
      bType = reflType (undefined :: Expr b)
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
   in case reflType (undefined :: Expr a) of
        ArrayType n t -> "array<" <> typeToWGSL' t <> ", " <> T.pack (show n) <> ">(" <> elements <> ")"
        _ -> undefined
exprToWGSL (ConstantVec @n @a' (_ :: Proxy a) x) =
  let elements = printVecElements @n @a' x
   in case reflType (undefined :: Expr a) of
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

class ReflType (a :: Types) where
  reflType :: Expr a -> Types

instance ReflType (Primitive TInt) where
  reflType _ = Primitive TInt

instance ReflType (Primitive TFloat) where
  reflType _ = Primitive TFloat

instance ReflType (Primitive TUInt) where
  reflType _ = Primitive TUInt

instance (KnownNat n, ReflType t) => ReflType (ArrayType (n :: Nat) t) where
  reflType _ = ArrayType (fromInteger $ natVal (Proxy @n)) (reflType (undefined :: Expr t))

instance ReflType (VectorType VL2 TInt) where
  reflType _ = VectorType VL2 TInt

instance ReflType (VectorType VL2 TFloat) where
  reflType _ = VectorType VL2 TFloat

instance ReflType (VectorType VL2 TUInt) where
  reflType _ = VectorType VL2 TUInt

instance ReflType (VectorType VL3 TInt) where
  reflType _ = VectorType VL3 TInt

instance ReflType (VectorType VL3 TFloat) where
  reflType _ = VectorType VL3 TFloat

instance ReflType (VectorType VL3 TUInt) where
  reflType _ = VectorType VL3 TUInt

instance ReflType (VectorType VL4 TInt) where
  reflType _ = VectorType VL4 TInt

instance ReflType (VectorType VL4 TFloat) where
  reflType _ = VectorType VL4 TFloat

instance ReflType (VectorType VL4 TUInt) where
  reflType _ = VectorType VL4 TUInt

typeToWGSL :: (ReflType a) => Expr a -> Text
typeToWGSL = typeToWGSL' . reflType

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
  Custom _ -> undefined

addStmt :: Stmt -> Shader ()
addStmt stmt = do
  state <- get
  put state {ast = Concat state.ast stmt}

newVarIndex :: Shader VarIndex
newVarIndex = do
  ShaderState {counter, ast} <- get
  put (ShaderState {counter = counter + 1, ast})
  pure counter

var :: Expr a -> Shader (Expr a)
var expr = do
  index <- newVarIndex
  addStmt (Let index expr)
  pure $ Var index

at :: forall n a. Expr (ArrayType n a) -> Expr (Primitive TUInt) -> Expr a
at = Index
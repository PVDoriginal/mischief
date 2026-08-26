{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeAbstractions #-}

module Mischief.Render.Shader where

import Control.Monad.State
import Data.Data hiding (cast)
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits (KnownNat, Nat, natVal)
import Mischief.ECS.Log (text)
import Mischief.Render.Shader.Bindings
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

data Types = Primitive PrimitiveTypes | ArrayType Nat PrimitiveTypes | VectorType VecLength PrimitiveTypes deriving (Eq)

data Expr (a :: Types) where
  Function :: Text -> [Expr a] -> Expr a
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
  ConstantArray :: forall (n :: Nat) (a' :: PrimitiveTypes). (PrintArrayElements n a', ReflType (ArrayType n a')) => Proxy (ArrayType n a') -> ArrayInit n a' -> Expr (ArrayType n a')
  ConstantVec :: forall (n :: VecLength) (a' :: PrimitiveTypes). (PrintVecElements n a', ReflType (VectorType n a')) => Proxy (VectorType n a') -> VecInit n a' -> Expr (VectorType n a')
  Var :: VarIndex -> Expr a
  BindingVar :: Integer -> Expr a
  Void :: Expr a

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
          VectorType VL2 TFloat -> unsafeCoerce $ vec2 @TFloat (fromInteger i, fromInteger i)
          VectorType VL2 TInt -> unsafeCoerce $ vec2 @TInt (fromInteger i, fromInteger i)
          VectorType VL2 TUInt -> unsafeCoerce $ vec2 @TUInt (fromInteger i, fromInteger i)
          VectorType VL3 TFloat -> unsafeCoerce $ vec3 @TFloat (fromInteger i, fromInteger i, fromInteger i)
          VectorType VL3 TInt -> unsafeCoerce $ vec3 @TInt (fromInteger i, fromInteger i, fromInteger i)
          VectorType VL3 TUInt -> unsafeCoerce $ vec3 @TUInt (fromInteger i, fromInteger i, fromInteger i)
          VectorType VL4 TFloat -> unsafeCoerce $ vec4 @TFloat (fromInteger i, fromInteger i, fromInteger i, fromInteger i)
          VectorType VL4 TInt -> unsafeCoerce $ vec4 @TInt (fromInteger i, fromInteger i, fromInteger i, fromInteger i)
          VectorType VL4 TUInt -> unsafeCoerce $ vec4 @TUInt (fromInteger i, fromInteger i, fromInteger i, fromInteger i)
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

float :: Float -> F32
float = ConstantFloat

int :: Int -> I32
int = ConstantInt

uint :: Int -> U32
uint = ConstantUInt

f32 :: Float -> F32
f32 = float

i32 :: Int -> I32
i32 = int

u32 :: Int -> U32
u32 = uint

vec2 :: forall (a :: PrimitiveTypes). (ReflType (VectorType VL2 a)) => VecInit VL2 a -> Vec2 a
vec2 = ConstantVec (Proxy @(VectorType VL2 a))

vec2f :: (Expr (Primitive TFloat), Expr (Primitive TFloat)) -> Vec2 TFloat
vec2f = vec2 @TFloat

vec2i :: (Expr (Primitive TInt), Expr (Primitive TInt)) -> Vec2 TInt
vec2i = vec2 @TInt

vec2u :: (Expr (Primitive TUInt), Expr (Primitive TUInt)) -> Vec2 TUInt
vec2u = vec2 @TUInt

vec3 :: forall (a :: PrimitiveTypes). (ReflType (VectorType VL3 a)) => VecInit VL3 a -> Vec3 a
vec3 = ConstantVec (Proxy @(VectorType VL3 a))

vec3f :: (Expr (Primitive TFloat), Expr (Primitive TFloat), Expr (Primitive TFloat)) -> Vec3 TFloat
vec3f = vec3 @TFloat

vec3i :: (Expr (Primitive TInt), Expr (Primitive TInt), Expr (Primitive TInt)) -> Vec3 TInt
vec3i = vec3 @TInt

vec3u :: (Expr (Primitive TUInt), Expr (Primitive TUInt), Expr (Primitive TUInt)) -> Vec3 TUInt
vec3u = vec3 @TUInt

vec4 :: forall (a :: PrimitiveTypes). (ReflType (VectorType VL4 a)) => VecInit VL4 a -> Vec4 a
vec4 = ConstantVec (Proxy @(VectorType VL4 a))

vec4f :: (Expr (Primitive TFloat), Expr (Primitive TFloat), Expr (Primitive TFloat), Expr (Primitive TFloat)) -> Vec4 TFloat
vec4f = vec4 @TFloat

vec4i :: (Expr (Primitive TInt), Expr (Primitive TInt), Expr (Primitive TInt), Expr (Primitive TInt)) -> Vec4 TInt
vec4i = vec4 @TInt

vec4u :: (Expr (Primitive TUInt), Expr (Primitive TUInt), Expr (Primitive TUInt), Expr (Primitive TUInt)) -> Vec4 TUInt
vec4u = vec4 @TUInt

array :: forall (a :: PrimitiveTypes) (n :: Nat). (PrintArrayElements n a, ReflType (ArrayType n a)) => ArrayInit n a -> Array n a
array = ConstantArray Proxy

arrayf :: forall (n :: Nat). (PrintArrayElements n TFloat, ReflType (ArrayType n TFloat)) => ArrayInit n TFloat -> Array n TFloat
arrayf = array @TFloat @n

arrayi :: forall (n :: Nat). (PrintArrayElements n TInt, ReflType (ArrayType n TInt)) => ArrayInit n TInt -> Array n TInt
arrayi = array @TInt @n

arrayu :: forall (n :: Nat). (PrintArrayElements n TUInt, ReflType (ArrayType n TUInt)) => ArrayInit n TUInt -> Array n TUInt
arrayu = array @TUInt @n

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

type family ArrayInit (n :: Nat) (a :: PrimitiveTypes) where
  ArrayInit 2 a = (Expr (Primitive a), Expr (Primitive a))
  ArrayInit 3 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))
  ArrayInit 4 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))
  ArrayInit 5 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))
  ArrayInit 6 a = (Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a), Expr (Primitive a))

class PrintArrayElements (n :: Nat) (a :: PrimitiveTypes) where
  printArrayElements :: ArrayInit n a -> Text

instance PrintArrayElements 2 a where
  printArrayElements (a, b) = exprToWGSL a <> ", " <> exprToWGSL b

instance PrintArrayElements 3 a where
  printArrayElements (a, b, c) = exprToWGSL a <> ", " <> exprToWGSL b <> ", " <> exprToWGSL c

-- toInt :: (ReflType a) => Expr a -> I32
-- toInt = cast @(Primitive TInt)

-- toFloat :: (ReflType a) => Expr a -> F32
-- toFloat = cast @(Primitive TFloat)

-- toUInt :: (ReflType a) => Expr a -> U32
-- toUInt = cast @(Primitive TUInt)

-- cast :: forall b a. (ReflType a, ReflType b) => Expr a -> Expr b
-- cast = Cast (Proxy @b)

class (ReflType a, ReflType b) => Cast a b where
  cast :: Expr a -> Expr b

instance (ReflType (Primitive a), ReflType (Primitive b)) => Cast (Primitive a) (Primitive b) where
  cast = Cast (Proxy @(Primitive b))

instance (ReflType (VectorType n a), ReflType (VectorType n b)) => Cast (VectorType n a) (VectorType n b) where
  cast = CastVec (Proxy @(VectorType n b))

showArgs :: [Expr a] -> Text
showArgs [] = ""
showArgs [x] = exprToWGSL x
showArgs (x : xs) = exprToWGSL x <> ", " <> showArgs xs

exprToWGSL :: Expr a -> Text
exprToWGSL (Function t x) = t <> "(" <> showArgs x <> ")"
exprToWGSL (Add a b) = "(" <> exprToWGSL a <> " + " <> exprToWGSL b <> ")"
exprToWGSL (Mult a b) = "(" <> exprToWGSL a <> " * " <> exprToWGSL b <> ")"
exprToWGSL (Sub a b) = "(" <> exprToWGSL a <> " - " <> exprToWGSL b <> ")"
exprToWGSL (Div a b) = "(" <> exprToWGSL a <> " / " <> exprToWGSL b <> ")"
exprToWGSL (Neg a) = "(-" <> exprToWGSL a <> ")"
exprToWGSL (Abs a) = exprToWGSL (Function "abs" [a])
exprToWGSL (Signum a) = exprToWGSL (Function "sign" [a])
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
        ArrayType n TInt -> "array<i32, " <> T.pack (show n) <> ">(" <> elements <> ")"
        ArrayType n TFloat -> "array<f32, " <> T.pack (show n) <> ">(" <> elements <> ")"
        ArrayType n TUInt -> "array<u32, " <> T.pack (show n) <> ">(" <> elements <> ")"
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
        VectorType VL4 TInt -> "vec3<i32>(" <> elements <> ")"
        VectorType VL4 TFloat -> "vec3<f32>(" <> elements <> ")"
        VectorType VL4 TUInt -> "vec3<u32>(" <> elements <> ")"
        _ -> undefined
exprToWGSL (ConstantInt x) = T.pack $ show x <> "i"
exprToWGSL (ConstantFloat x) = T.pack $ show x <> "f"
exprToWGSL (ConstantUInt x) = T.pack $ show x <> "u"
exprToWGSL (Var x) = nameIndex x
exprToWGSL (BindingVar x) = "b" <> T.pack (show x)
exprToWGSL Void = ""

stmtToWGSL :: Stmt -> Text
stmtToWGSL Empty = ""
stmtToWGSL (Concat Empty a) = stmtToWGSL a
stmtToWGSL (Concat a b) = stmtToWGSL a <> "\n" <> stmtToWGSL b
stmtToWGSL (Let a b) = "  let " <> nameIndex a <> " = " <> exprToWGSL b <> ";"
stmtToWGSL (Return a) = "  return " <> exprToWGSL a <> ";"

newtype Shader a = Shader (State ShaderState a) deriving newtype (Functor, Applicative, Monad, MonadState ShaderState)

shaderToWGSL :: Shader (Expr a) -> Text
shaderToWGSL (Shader s) = do
  let (a, ShaderState {ast}) = runState s ShaderState {counter = 0, ast = Empty}
  let ast' = Concat ast (Return a)
  stmtToWGSL ast'

class ReflType (a :: Types) where
  reflType :: Expr a -> Types

instance ReflType (Primitive TInt) where
  reflType _ = Primitive TInt

instance ReflType (Primitive TFloat) where
  reflType _ = Primitive TFloat

instance ReflType (Primitive TUInt) where
  reflType _ = Primitive TUInt

instance (KnownNat n) => ReflType (ArrayType (n :: Nat) TInt) where
  reflType _ = ArrayType (fromInteger $ natVal (Proxy @n)) TInt

instance (KnownNat n) => ReflType (ArrayType (n :: Nat) TFloat) where
  reflType _ = ArrayType (fromInteger $ natVal (Proxy @n)) TFloat

instance (KnownNat n) => ReflType (ArrayType (n :: Nat) TUInt) where
  reflType _ = ArrayType (fromInteger $ natVal (Proxy @n)) TUInt

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

type I32 = Expr (Primitive TInt)

type F32 = Expr (Primitive TFloat)

type U32 = Expr (Primitive TUInt)

type Array (a :: Nat) b = Expr (ArrayType a b)

type Vec2 a = Expr (VectorType VL2 a)

type Vec2f = Expr (VectorType VL2 TFloat)

type Vec2i = Expr (VectorType VL2 TInt)

type Vec2u = Expr (VectorType VL2 TUInt)

type Vec3 a = Expr (VectorType VL3 a)

type Vec3f = Expr (VectorType VL3 TFloat)

type Vec3i = Expr (VectorType VL3 TInt)

type Vec3u = Expr (VectorType VL3 TUInt)

type Vec4 a = Expr (VectorType VL4 a)

type Vec4f = Expr (VectorType VL4 TFloat)

type Vec4i = Expr (VectorType VL4 TInt)

type Vec4u = Expr (VectorType VL4 TUInt)

-- newtype Value a = Value Expr deriving newtype (Show)

typeToWGSL :: (ReflType a) => Expr a -> Text
typeToWGSL a = case reflType a of
  Primitive TInt -> "i32"
  Primitive TFloat -> "f32"
  Primitive TUInt -> "u32"
  ArrayType n TFloat -> "array<f32, " <> T.pack (show n) <> ">"
  ArrayType n TInt -> "array<i32, " <> T.pack (show n) <> ">"
  ArrayType n TUInt -> "array<u32, " <> T.pack (show n) <> ">"
  VectorType VL2 TFloat -> "vec2<f32>"
  VectorType VL2 TInt -> "vec2<i32>"
  VectorType VL2 TUInt -> "vec2<u32>"
  VectorType VL3 TFloat -> "vec3<f32>"
  VectorType VL3 TInt -> "vec3<i32>"
  VectorType VL3 TUInt -> "vec3<u32>"
  VectorType VL4 TFloat -> "vec4<f32>"
  VectorType VL4 TInt -> "vec4<i32>"
  VectorType VL4 TUInt -> "vec4<u32>"

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

putBindings :: (Bindable b) => b -> Shader ()
putBindings = undefined

--   pure . Value . CodeString $ nameIndex index

-- plus :: Value a -> Value a -> Shader (Value a)
-- plus a b = mkLet (CodeString $ show a ++ " + " ++ show b)

abs' :: Expr a -> Expr a
abs' = Abs

sin :: F32 -> F32
sin x = Function "sin" [x]

cos :: F32 -> F32
cos x = Function "cos" [x]

test :: Shader Vec2f
test = do
  x <- var $ vec2 @TInt (2, 3)
  -- y <- var $ abs' x
  -- pure $ 1 + y
  pure $ cast x

test2 :: Shader F32
test2 = do
  let x = 2 + 3
  let y = x / 2

  let s = sin y
  let c = cos (y + 3)

  let x = 5
  let y = x / 2.0

  w :: F32 <- var y

  pure $ s + c

sample :: forall n m. (KnownNat n, KnownNat m) => Binding n Sampler -> Binding m Texture2d -> F32
sample _ _ = Function "sampleTexture" [BindingVar $ natVal (Proxy @n), BindingVar $ natVal (Proxy @m)]

genFunction :: forall a. (ReflType a) => Text -> Shader (Expr a) -> Text
genFunction name s = "fn " <> name <> "() -> " <> typeToWGSL (undefined :: Expr a) <> " {\n" <> shaderToWGSL s <> "\n}"

gen :: forall a b. (ReflType a, Bindable b) => (b -> Shader (Expr a)) -> Text
gen s = genBindings (Proxy @b) <> genFunction "main" (s def)

genBindings :: forall b. (Bindable b) => Proxy b -> Text
genBindings _ =
  let Bindings x = collectBindings (Proxy @b)
   in T.concat (map genBinding x)

genBinding :: BindingData -> Text
genBinding BindingData {bType, index} = "@group(0) @binding(" <> T.pack (show index) <> ")\nvar b" <> T.pack (show index) <> " : " <> genBindingType bType <> ";\n\n"

genBindingType :: BindingType -> Text
genBindingType BSampler = "sampler"
genBindingType BTexture2d = "texture_2d<f32>"

data Test = Test
  { sampler :: Binding 0 Sampler,
    texture :: Binding 1 Texture2d
  }
  deriving (Generic, Bindable, Default)

test3 :: Test -> Shader F32
test3 test = do
  pure $ sample test.sampler test.texture

genShader :: (Bindable b, ReflType a) => (b -> Shader (Expr a)) -> String
genShader = T.unpack . gen

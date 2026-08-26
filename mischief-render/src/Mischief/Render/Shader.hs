{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Mischief.Render.Shader where

import Control.Monad.State
import Data.Data hiding (cast)
import Data.Default
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)
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

-- instance Show Stmt where
--   show Empty = ""
--   show (Concat Empty b) = show b
--   show (Concat a b) = show a ++ "\n" ++ show b
--   show (Let a b) = "v" ++ show a ++ " = " ++ show b

data Types = TInt | TFloat | TUInt deriving (Eq)

data Expr (a :: Types) where
  Function :: Text -> [Expr a] -> Expr a
  Add :: Expr a -> Expr a -> Expr a
  Sub :: Expr a -> Expr a -> Expr a
  Mult :: Expr a -> Expr a -> Expr a
  Div :: Expr a -> Expr a -> Expr a
  ModI :: Expr TInt -> Expr TInt -> Expr TInt
  ModU :: Expr TUInt -> Expr TUInt -> Expr TUInt
  Neg :: Expr a -> Expr a
  Abs :: Expr a -> Expr a
  Signum :: Expr a -> Expr a
  Cast :: (ReflType a, ReflType b) => Proxy b -> Expr a -> Expr b
  ConstantInt :: Int -> Expr TInt
  ConstantFloat :: Float -> Expr TFloat
  ConstantUInt :: Int -> Expr TUInt
  Var :: VarIndex -> Expr a
  BindingVar :: Integer -> Expr a
  Void :: Expr a

instance (ReflType a) => Num (Expr (a :: Types)) where
  (+) = Add
  (*) :: Expr a -> Expr a -> Expr a
  (*) = Mult
  abs = Abs
  signum = Signum
  fromInteger i =
    let t = reflType (undefined :: Expr a)
     in case t of
          TInt -> unsafeCoerce $ ConstantInt (fromInteger i)
          TFloat -> unsafeCoerce $ ConstantFloat (fromInteger i)
          TUInt -> unsafeCoerce $ ConstantUInt (fromInteger i)
  negate = Neg

instance Fractional (Expr TFloat) where
  fromRational = ConstantFloat . fromRational
  recip = Div 1
  (/) = Div

instance (ReflType a) => Enum (Expr a) where
  toEnum i = fromInteger (toInteger i)
  fromEnum (ConstantInt x) = x
  fromEnum _ = undefined

float :: Float -> Expr TFloat
float = ConstantFloat

int :: Int -> Expr TInt
int = ConstantInt

uint :: Int -> Expr TUInt
uint = ConstantUInt

f32 :: Float -> Expr TFloat
f32 = float

i32 :: Int -> Expr TInt
i32 = int

u32 :: Int -> Expr TUInt
u32 = uint

toInt :: (ReflType a) => Expr a -> I32
toInt = cast @TInt

toFloat :: (ReflType a) => Expr a -> F32
toFloat = cast @TFloat

toUInt :: (ReflType a) => Expr a -> U32
toUInt = cast @TUInt

cast :: forall b a. (ReflType a, ReflType b) => Expr a -> Expr b
cast = Cast (Proxy @b)

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
exprToWGSL (ModI a b) = "(" <> exprToWGSL a <> " % " <> exprToWGSL b <> ")"
exprToWGSL (ModU a b) = "(" <> exprToWGSL a <> " % " <> exprToWGSL b <> ")"
exprToWGSL (Neg a) = "(-" <> exprToWGSL a <> ")"
exprToWGSL (Abs a) = exprToWGSL (Function "abs" [a])
exprToWGSL (Signum a) = exprToWGSL (Function "sign" [a])
exprToWGSL (Cast (_ :: Proxy b) (a :: Expr a)) =
  let aType = reflType (undefined :: Expr a)
      bType = reflType (undefined :: Expr b)
   in if aType == bType
        then exprToWGSL a
        else case bType of
          TInt -> "i32(" <> exprToWGSL a <> ")"
          TFloat -> "f32(" <> exprToWGSL a <> ")"
          TUInt -> "u32(" <> exprToWGSL a <> ")"
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

instance ReflType TInt where
  reflType _ = TInt

instance ReflType TFloat where
  reflType _ = TFloat

instance ReflType TUInt where
  reflType _ = TUInt

type I32 = Expr TInt

type F32 = Expr TFloat

type U32 = Expr TUInt

-- newtype Value a = Value Expr deriving newtype (Show)

typeToWGSL :: (ReflType a) => Expr a -> Text
typeToWGSL a = case reflType a of
  TInt -> "i32"
  TFloat -> "f32"
  TUInt -> "u32"

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

test :: Shader F32
test = do
  let x = 2 + 3
  y <- var $ abs' x
  pure $ 1 + y

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
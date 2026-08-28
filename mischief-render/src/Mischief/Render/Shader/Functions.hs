{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Mischief.Render.Shader.Functions where

import Data.Data
import Data.Singletons (SingI)
import GHC.TypeLits
import Mischief.Render.Core
import Mischief.Render.Shader.Bindings
import Mischief.Render.Shader.Singletons
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Texture

sample :: Expr TTexture -> Expr TSampler -> Vec2f -> Vec4f
sample tex sampler v =
  Function
    "textureSample"
    [ Param tex,
      Param sampler,
      Param v
    ]

-- Numeric Functions

abs :: (SingI a, IsAlgebric a ~ True) => Expr a -> Expr a
abs = Abs

acos :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
acos x = Function "acos" [Param x]

acosh :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
acosh x = Function "acosh" [Param x]

asin :: (SingI a, IsAlgebric a ~ True) => Expr a -> Expr a
asin x = Function "asin" [Param x]

asinh :: (SingI a, IsAlgebric a ~ True) => Expr a -> Expr a
asinh x = Function "asinh" [Param x]

atan :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
atan x = Function "atan" [Param x]

atanh :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
atanh x = Function "atanh" [Param x]

atan2 :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a -> Expr a
atan2 x y = Function "atan2" [Param x, Param y]

ceil :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
ceil x = Function "ceil" [Param x]

clamp :: (SingI a, IsAlgebric a ~ True) => Expr a -> Expr a -> Expr a -> Expr a
clamp e low high = Function "clamp" [Param e, Param low, Param high]

cos :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
cos x = Function "cos" [Param x]

cosh :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
cosh x = Function "cosh" [Param x]

countLeadingZeros :: (SingI a, IsIntegral a ~ True) => Expr a -> Expr a
countLeadingZeros x = Function "countLeadingZeros" [Param x]

countOneBits :: (SingI a, IsIntegral a ~ True) => Expr a -> Expr a
countOneBits x = Function "countOneBits" [Param x]

countTrailingZeros :: (SingI a, IsIntegral a ~ True) => Expr a -> Expr a
countTrailingZeros x = Function "countTrailingZeros" [Param x]

cross :: (SingI (VectorType VL3 a), Fractional (Expr (Primitive a))) => Expr (VectorType VL3 a) -> Expr (VectorType VL3 a) -> Expr (VectorType VL3 a)
cross a b = Function "cross" [Param a, Param b]

degrees :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
degrees x = Function "degrees" [Param x]

-- TODO: determinant (matrix)

distance :: (SingI a, Fractional (Expr a), VectorOrSingleOf a ~ b) => Expr a -> Expr a -> Expr (Primitive b)
distance a b = Function "distance" [Param a, Param b]

dot :: (SingI (Expr (VectorType n a)), SingI a, SingI n, IsAlgebric (Primitive a) ~ True) => Expr (VectorType n a) -> Expr (VectorType n a) -> Expr (Primitive a)
dot a b = Function "dot" [Param a, Param b]

dot4U8Packed :: U32 -> U32 -> U32
dot4U8Packed a b = Function "dot4U8Packed" [Param a, Param b]

dot4I8Packed :: U32 -> U32 -> I32
dot4I8Packed a b = Function "dot4I8Packed" [Param a, Param b]

exp :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
exp a = Function "exp" [Param a]

exp2 :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
exp2 a = Function "exp2" [Param a]

extractBits :: (SingI a, IsIntegral a ~ True) => Expr a -> U32 -> U32 -> Expr a
extractBits e offset count = Function "extractBits" [Param e, Param offset, Param count]

faceForward :: (SingI a, SingI n, Fractional (Expr (Primitive a))) => Expr (VectorType n a) -> Expr (VectorType n a) -> Expr (VectorType n a) -> Expr (VectorType n a)
faceForward a b c = Function "faceForward" [Param a, Param b, Param c]

firstLeadingBit :: (SingI a, SingI n, IsIntegral a ~ True) => Expr a -> Expr a
firstLeadingBit a = Function "firstLeadingBit" [Param a]

firstTrailingBit :: (SingI a, SingI n, IsIntegral a ~ True) => Expr a -> Expr a
firstTrailingBit a = Function "firstTrailingBit" [Param a]

floor :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
floor x = Function "floor" [Param x]

fma :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a -> Expr a -> Expr a
fma a b c = Function "fma" [Param a, Param b, Param c]

fract :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
fract a = Function "fract" [Param a]

-- TODO: frexp (custom struct)

insertBits :: (SingI a, IsIntegral a ~ True) => Expr a -> Expr a -> U32 -> U32 -> Expr a
insertBits e newbits offset count = Function "insertBits" [Param e, Param newbits, Param offset, Param count]

inverseSqrt :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
inverseSqrt a = Function "inverseSqrt" [Param a]

-- TODO: ldexp

length :: (SingI a, SingI b, SingI n, VectorOrSingleOf b ~ a) => Expr b -> Expr (Primitive a)
length a = Function "length" [Param a]

log :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
log a = Function "log" [Param a]

log2 :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a
log2 a = Function "log2" [Param a]

max :: (SingI a, IsAlgebric a ~ True) => Expr a -> Expr a -> Expr a
max a b = Function "max" [Param a, Param b]

min :: (SingI a, IsAlgebric a ~ True) => Expr a -> Expr a -> Expr a
min a b = Function "min" [Param a, Param b]

mix :: (SingI a, Fractional (Expr a)) => Expr a -> Expr a -> Expr a -> Expr a
mix a b c = Function "mix" [Param a, Param b, Param c]
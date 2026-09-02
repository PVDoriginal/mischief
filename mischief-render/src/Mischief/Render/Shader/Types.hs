{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mischief.Render.Shader.Types where

import Data.Data
import Data.Singletons (SingI)
import GHC.Records (HasField (getField))
import GHC.TypeLits
import Mischief.Render.Shader.Singletons
import Mischief.Render.Shader.State

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

vec2 :: forall (a :: PrimitiveTypes). (SingI a) => VecInit VL2 a -> Vec2 a
vec2 = ConstantVec (Proxy @(VectorType VL2 a))

vec2f :: (Expr (Primitive TFloat), Expr (Primitive TFloat)) -> Vec2 TFloat
vec2f = vec2 @TFloat

vec2i :: (Expr (Primitive TInt), Expr (Primitive TInt)) -> Vec2 TInt
vec2i = vec2 @TInt

vec2u :: (Expr (Primitive TUInt), Expr (Primitive TUInt)) -> Vec2 TUInt
vec2u = vec2 @TUInt

vec3 :: forall (a :: PrimitiveTypes). (SingI a) => VecInit VL3 a -> Vec3 a
vec3 = ConstantVec (Proxy @(VectorType VL3 a))

vec3f :: (Expr (Primitive TFloat), Expr (Primitive TFloat), Expr (Primitive TFloat)) -> Vec3 TFloat
vec3f = vec3 @TFloat

vec3i :: (Expr (Primitive TInt), Expr (Primitive TInt), Expr (Primitive TInt)) -> Vec3 TInt
vec3i = vec3 @TInt

vec3u :: (Expr (Primitive TUInt), Expr (Primitive TUInt), Expr (Primitive TUInt)) -> Vec3 TUInt
vec3u = vec3 @TUInt

vec4 :: forall (a :: PrimitiveTypes). (SingI a) => VecInit VL4 a -> Vec4 a
vec4 = ConstantVec (Proxy @(VectorType VL4 a))

vec4f :: (Expr (Primitive TFloat), Expr (Primitive TFloat), Expr (Primitive TFloat), Expr (Primitive TFloat)) -> Vec4 TFloat
vec4f = vec4 @TFloat

vec4i :: (Expr (Primitive TInt), Expr (Primitive TInt), Expr (Primitive TInt), Expr (Primitive TInt)) -> Vec4 TInt
vec4i = vec4 @TInt

vec4u :: (Expr (Primitive TUInt), Expr (Primitive TUInt), Expr (Primitive TUInt), Expr (Primitive TUInt)) -> Vec4 TUInt
vec4u = vec4 @TUInt

array :: forall (n :: Nat) (a :: Types). (PrintArrayElements n a, SingI (ArrayType n a)) => ArrayInit n a -> Array n a
array = ConstantArray Proxy

arrayf :: forall (n :: Nat). (PrintArrayElements n (Primitive TFloat), SingI (ArrayType n (Primitive TFloat))) => ArrayInit n (Primitive TFloat) -> Array n (Primitive TFloat)
arrayf = array @n @(Primitive TFloat)

arrayi :: forall (n :: Nat). (PrintArrayElements n (Primitive TInt), SingI (ArrayType n (Primitive TInt))) => ArrayInit n (Primitive TInt) -> Array n (Primitive TInt)
arrayi = array @n @(Primitive TInt)

arrayu :: forall (n :: Nat). (PrintArrayElements n (Primitive TUInt), SingI (ArrayType n (Primitive TUInt))) => ArrayInit n (Primitive TUInt) -> Array n (Primitive TUInt)
arrayu = array @n @(Primitive TUInt)

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

type Mat2x2 = Expr (MatrixType ML2 VL2)

type Mat2x3 = Expr (MatrixType ML3 VL2)

type Mat2x4 = Expr (MatrixType ML4 VL2)

type Mat3x2 = Expr (MatrixType ML2 VL3)

type Mat3x3 = Expr (MatrixType ML3 VL3)

type Mat3x4 = Expr (MatrixType ML4 VL3)

type Mat4x2 = Expr (MatrixType ML2 VL4)

type Mat4x3 = Expr (MatrixType ML3 VL4)

type Mat4x4 = Expr (MatrixType ML4 VL4)

mat2x2 :: MatInit ML2 VL2 -> Mat2x2
mat2x2 = ConstantMat (Proxy @(MatrixType ML2 VL2))

mat2x3 :: MatInit ML3 VL2 -> Mat2x3
mat2x3 = ConstantMat (Proxy @(MatrixType ML3 VL2))

mat2x4 :: MatInit ML4 VL2 -> Mat2x4
mat2x4 = ConstantMat (Proxy @(MatrixType ML4 VL2))

mat3x2 :: MatInit ML2 VL3 -> Mat3x2
mat3x2 = ConstantMat (Proxy @(MatrixType ML2 VL3))

mat3x3 :: MatInit ML3 VL3 -> Mat3x3
mat3x3 = ConstantMat (Proxy @(MatrixType ML3 VL3))

mat3x4 :: MatInit ML4 VL3 -> Mat3x4
mat3x4 = ConstantMat (Proxy @(MatrixType ML4 VL3))

mat4x2 :: MatInit ML2 VL4 -> Mat4x2
mat4x2 = ConstantMat (Proxy @(MatrixType ML2 VL4))

mat4x3 :: MatInit ML3 VL4 -> Mat4x3
mat4x3 = ConstantMat (Proxy @(MatrixType ML3 VL4))

mat4x4 :: MatInit ML4 VL4 -> Mat4x4
mat4x4 = ConstantMat (Proxy @(MatrixType ML4 VL4))

class (SingI b, SingI a) => Cast b a where
  cast :: Expr a -> Expr b

instance (SingI (Primitive a), SingI (Primitive b)) => Cast (Primitive a) (Primitive b) where
  cast = Cast (Proxy @(Primitive a))

instance (SingI (VectorType n a), SingI (VectorType n b)) => Cast (VectorType n a) (VectorType n b) where
  cast = CastVec (Proxy @(VectorType n a))

type family ThreeElements (a :: VecLength) where
  ThreeElements VL2 = False
  ThreeElements a = True

type family FourElements (a :: VecLength) where
  FourElements VL4 = True
  FourElements a = False

instance (SingI (VectorType n a)) => HasField "x" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "x"

instance (SingI (VectorType n a)) => HasField "r" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "r"

instance (SingI (VectorType n a)) => HasField "y" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "y"

instance (SingI (VectorType n a)) => HasField "g" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "g"

instance (ThreeElements n ~ True, SingI (VectorType n a)) => HasField "z" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "z"

instance (ThreeElements n ~ True, SingI (VectorType n a)) => HasField "b" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "b"

instance (FourElements n ~ True, SingI (VectorType n a)) => HasField "w" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "w"

instance (FourElements n ~ True, SingI (VectorType n a)) => HasField "a" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "a"

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Mischief.Render.Shader.Types where

import Data.Data
import GHC.Records (HasField (getField))
import GHC.TypeLits
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

array :: forall (n :: Nat) (a :: Types). (PrintArrayElements n a, ReflType (ArrayType n a)) => ArrayInit n a -> Array n a
array = ConstantArray Proxy

arrayf :: forall (n :: Nat). (PrintArrayElements n (Primitive TFloat), ReflType (ArrayType n (Primitive TFloat))) => ArrayInit n (Primitive TFloat) -> Array n (Primitive TFloat)
arrayf = array @n @(Primitive TFloat)

arrayi :: forall (n :: Nat). (PrintArrayElements n (Primitive TInt), ReflType (ArrayType n (Primitive TInt))) => ArrayInit n (Primitive TInt) -> Array n (Primitive TInt)
arrayi = array @n @(Primitive TInt)

arrayu :: forall (n :: Nat). (PrintArrayElements n (Primitive TUInt), ReflType (ArrayType n (Primitive TUInt))) => ArrayInit n (Primitive TUInt) -> Array n (Primitive TUInt)
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

class (ReflType a, ReflType b) => Cast a b where
  cast :: Expr a -> Expr b

instance (ReflType (Primitive a), ReflType (Primitive b)) => Cast (Primitive a) (Primitive b) where
  cast = Cast (Proxy @(Primitive b))

instance (ReflType (VectorType n a), ReflType (VectorType n b)) => Cast (VectorType n a) (VectorType n b) where
  cast = CastVec (Proxy @(VectorType n b))

type family ThreeElements (a :: VecLength) where
  ThreeElements VL2 = False
  ThreeElements a = True

type family FourElements (a :: VecLength) where
  FourElements VL4 = True
  FourElements a = False

instance HasField "x" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "x"

instance HasField "r" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "r"

instance HasField "y" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "y"

instance HasField "g" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "g"

instance HasField "z" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "z"

instance HasField "b" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "b"

instance (FourElements n ~ True) => HasField "w" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "w"

instance (FourElements n ~ True) => HasField "a" (Expr (VectorType n a)) (Expr (Primitive a)) where
  getField v = AccessField v "a"

instance HasField "xx" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "xx"

instance HasField "rr" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "rr"

instance HasField "xy" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "xy"

instance HasField "rg" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "rg"

instance HasField "xz" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "xz"

instance HasField "rb" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "rb"

instance (FourElements n ~ True) => HasField "xw" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "xw"

instance (FourElements n ~ True) => HasField "ra" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "ra"

instance HasField "yx" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "yx"

instance HasField "gr" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "gr"

instance HasField "yy" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "yy"

instance HasField "gg" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "gg"

instance HasField "yz" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "yz"

instance HasField "gb" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "gb"

instance (FourElements n ~ True) => HasField "yw" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "yw"

instance (FourElements n ~ True) => HasField "ga" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "ga"

instance HasField "zx" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "zx"

instance HasField "br" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "br"

instance HasField "zy" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "zy"

instance HasField "bg" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "bg"

instance HasField "zz" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "zz"

instance HasField "bb" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "bb"

instance (FourElements n ~ True) => HasField "zw" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "zw"

instance (FourElements n ~ True) => HasField "ba" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "ba"

instance (FourElements n ~ True) => HasField "wx" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "wx"

instance (FourElements n ~ True) => HasField "ar" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "ar"

instance (FourElements n ~ True) => HasField "wy" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "wy"

instance (FourElements n ~ True) => HasField "ag" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "ag"

instance (FourElements n ~ True) => HasField "wz" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "wz"

instance (FourElements n ~ True) => HasField "ab" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "ab"

instance (FourElements n ~ True) => HasField "ww" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "ww"

instance (FourElements n ~ True) => HasField "aa" (Expr (VectorType n a)) (Expr (VectorType VL2 a)) where
  getField v = AccessField v "aa"

instance HasField "xxx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xxx"

instance HasField "rrr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rrr"

instance HasField "xxy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xxy"

instance HasField "rrg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rrg"

instance HasField "xxz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xxz"

instance HasField "rrb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rrb"

instance (FourElements n ~ True) => HasField "xxw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xxw"

instance (FourElements n ~ True) => HasField "rra" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rra"

instance HasField "xyx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xyx"

instance HasField "rgr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rgr"

instance HasField "xyy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xyy"

instance HasField "rgg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rgg"

instance HasField "xyz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xyz"

instance HasField "rgb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rgb"

instance (FourElements n ~ True) => HasField "xyw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xyw"

instance (FourElements n ~ True) => HasField "rga" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rga"

instance HasField "xzx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xzx"

instance HasField "rbr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rbr"

instance HasField "xzy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xzy"

instance HasField "rbg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rbg"

instance HasField "xzz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xzz"

instance HasField "rbb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rbb"

instance (FourElements n ~ True) => HasField "xzw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xzw"

instance (FourElements n ~ True) => HasField "rba" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rba"

instance (FourElements n ~ True) => HasField "xwx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xwx"

instance (FourElements n ~ True) => HasField "rar" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rar"

instance (FourElements n ~ True) => HasField "xwy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xwy"

instance (FourElements n ~ True) => HasField "rag" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rag"

instance (FourElements n ~ True) => HasField "xwz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xwz"

instance (FourElements n ~ True) => HasField "rab" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "rab"

instance (FourElements n ~ True) => HasField "xww" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "xww"

instance (FourElements n ~ True) => HasField "raa" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "raa"

instance HasField "yxx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yxx"

instance HasField "grr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "grr"

instance HasField "yxy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yxy"

instance HasField "grg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "grg"

instance HasField "yxz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yxz"

instance HasField "grb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "grb"

instance (FourElements n ~ True) => HasField "yxw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yxw"

instance (FourElements n ~ True) => HasField "gra" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gra"

instance HasField "yyx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yyx"

instance HasField "ggr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "ggr"

instance HasField "yyy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yyy"

instance HasField "ggg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "ggg"

instance HasField "yyz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yyz"

instance HasField "ggb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "ggb"

instance (FourElements n ~ True) => HasField "yyw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yyw"

instance (FourElements n ~ True) => HasField "gga" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gga"

instance HasField "yzx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yzx"

instance HasField "gbr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gbr"

instance HasField "yzy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yzy"

instance HasField "gbg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gbg"

instance HasField "yzz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yzz"

instance HasField "gbb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gbb"

instance (FourElements n ~ True) => HasField "yzw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yzw"

instance (FourElements n ~ True) => HasField "gba" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gba"

instance (FourElements n ~ True) => HasField "ywx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "ywx"

instance (FourElements n ~ True) => HasField "gar" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gar"

instance (FourElements n ~ True) => HasField "ywy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "ywy"

instance (FourElements n ~ True) => HasField "gag" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gag"

instance (FourElements n ~ True) => HasField "ywz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "ywz"

instance (FourElements n ~ True) => HasField "gab" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gab"

instance (FourElements n ~ True) => HasField "yww" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "yww"

instance (FourElements n ~ True) => HasField "gaa" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "gaa"

instance HasField "zxx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zxx"

instance HasField "brr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "brr"

instance HasField "zxy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zxy"

instance HasField "brg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "brg"

instance HasField "zxz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zxz"

instance HasField "brb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "brb"

instance (FourElements n ~ True) => HasField "zxw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zxw"

instance (FourElements n ~ True) => HasField "bra" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bra"

instance HasField "zyx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zyx"

instance HasField "bgr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bgr"

instance HasField "zyy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zyy"

instance HasField "bgg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bgg"

instance HasField "zyz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zyz"

instance HasField "bgb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bgb"

instance (FourElements n ~ True) => HasField "zyw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zyw"

instance (FourElements n ~ True) => HasField "bga" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bga"

instance HasField "zzx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zzx"

instance HasField "bbr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bbr"

instance HasField "zzy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zzy"

instance HasField "bbg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bbg"

instance HasField "zzz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zzz"

instance HasField "bbb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bbb"

instance (FourElements n ~ True) => HasField "zzw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zzw"

instance (FourElements n ~ True) => HasField "bba" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bba"

instance (FourElements n ~ True) => HasField "zwx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zwx"

instance (FourElements n ~ True) => HasField "bar" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bar"

instance (FourElements n ~ True) => HasField "zwy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zwy"

instance (FourElements n ~ True) => HasField "bag" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bag"

instance (FourElements n ~ True) => HasField "zwz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zwz"

instance (FourElements n ~ True) => HasField "bab" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "bab"

instance (FourElements n ~ True) => HasField "zww" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "zww"

instance (FourElements n ~ True) => HasField "baa" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "baa"

instance (FourElements n ~ True) => HasField "wxx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wxx"

instance (FourElements n ~ True) => HasField "arr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "arr"

instance (FourElements n ~ True) => HasField "wxy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wxy"

instance (FourElements n ~ True) => HasField "arg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "arg"

instance (FourElements n ~ True) => HasField "wxz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wxz"

instance (FourElements n ~ True) => HasField "arb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "arb"

instance (FourElements n ~ True) => HasField "wxw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wxw"

instance (FourElements n ~ True) => HasField "ara" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "ara"

instance (FourElements n ~ True) => HasField "wyx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wyx"

instance (FourElements n ~ True) => HasField "agr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "agr"

instance (FourElements n ~ True) => HasField "wyy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wyy"

instance (FourElements n ~ True) => HasField "agg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "agg"

instance (FourElements n ~ True) => HasField "wyz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wyz"

instance (FourElements n ~ True) => HasField "agb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "agb"

instance (FourElements n ~ True) => HasField "wyw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wyw"

instance (FourElements n ~ True) => HasField "aga" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "aga"

instance (FourElements n ~ True) => HasField "wzx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wzx"

instance (FourElements n ~ True) => HasField "abr" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "abr"

instance (FourElements n ~ True) => HasField "wzy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wzy"

instance (FourElements n ~ True) => HasField "abg" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "abg"

instance (FourElements n ~ True) => HasField "wzz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wzz"

instance (FourElements n ~ True) => HasField "abb" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "abb"

instance (FourElements n ~ True) => HasField "wzw" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wzw"

instance (FourElements n ~ True) => HasField "aba" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "aba"

instance (FourElements n ~ True) => HasField "wwx" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wwx"

instance (FourElements n ~ True) => HasField "aar" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "aar"

instance (FourElements n ~ True) => HasField "wwy" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wwy"

instance (FourElements n ~ True) => HasField "aag" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "aag"

instance (FourElements n ~ True) => HasField "wwz" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "wwz"

instance (FourElements n ~ True) => HasField "aab" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "aab"

instance (FourElements n ~ True) => HasField "www" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "www"

instance (FourElements n ~ True) => HasField "aaa" (Expr (VectorType n a)) (Expr (VectorType VL3 a)) where
  getField v = AccessField v "aaa"

instance HasField "xxxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxxx"

instance HasField "rrrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrrr"

instance HasField "xxxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxxy"

instance HasField "rrrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrrg"

instance HasField "xxxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxxz"

instance HasField "rrrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrrb"

instance (FourElements n ~ True) => HasField "xxxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxxw"

instance (FourElements n ~ True) => HasField "rrra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrra"

instance HasField "xxyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxyx"

instance HasField "rrgr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrgr"

instance HasField "xxyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxyy"

instance HasField "rrgg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrgg"

instance HasField "xxyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxyz"

instance HasField "rrgb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrgb"

instance (FourElements n ~ True) => HasField "xxyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxyw"

instance (FourElements n ~ True) => HasField "rrga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrga"

instance HasField "xxzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxzx"

instance HasField "rrbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrbr"

instance HasField "xxzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxzy"

instance HasField "rrbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrbg"

instance HasField "xxzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxzz"

instance HasField "rrbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrbb"

instance (FourElements n ~ True) => HasField "xxzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxzw"

instance (FourElements n ~ True) => HasField "rrba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrba"

instance (FourElements n ~ True) => HasField "xxwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxwx"

instance (FourElements n ~ True) => HasField "rrar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrar"

instance (FourElements n ~ True) => HasField "xxwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxwy"

instance (FourElements n ~ True) => HasField "rrag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrag"

instance (FourElements n ~ True) => HasField "xxwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxwz"

instance (FourElements n ~ True) => HasField "rrab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rrab"

instance (FourElements n ~ True) => HasField "xxww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xxww"

instance (FourElements n ~ True) => HasField "rraa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rraa"

instance HasField "xyxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyxx"

instance HasField "rgrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgrr"

instance HasField "xyxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyxy"

instance HasField "rgrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgrg"

instance HasField "xyxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyxz"

instance HasField "rgrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgrb"

instance (FourElements n ~ True) => HasField "xyxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyxw"

instance (FourElements n ~ True) => HasField "rgra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgra"

instance HasField "xyyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyyx"

instance HasField "rggr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rggr"

instance HasField "xyyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyyy"

instance HasField "rggg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rggg"

instance HasField "xyyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyyz"

instance HasField "rggb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rggb"

instance (FourElements n ~ True) => HasField "xyyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyyw"

instance (FourElements n ~ True) => HasField "rgga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgga"

instance HasField "xyzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyzx"

instance HasField "rgbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgbr"

instance HasField "xyzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyzy"

instance HasField "rgbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgbg"

instance HasField "xyzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyzz"

instance HasField "rgbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgbb"

instance (FourElements n ~ True) => HasField "xyzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyzw"

instance (FourElements n ~ True) => HasField "rgba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgba"

instance (FourElements n ~ True) => HasField "xywx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xywx"

instance (FourElements n ~ True) => HasField "rgar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgar"

instance (FourElements n ~ True) => HasField "xywy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xywy"

instance (FourElements n ~ True) => HasField "rgag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgag"

instance (FourElements n ~ True) => HasField "xywz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xywz"

instance (FourElements n ~ True) => HasField "rgab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgab"

instance (FourElements n ~ True) => HasField "xyww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xyww"

instance (FourElements n ~ True) => HasField "rgaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rgaa"

instance HasField "xzxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzxx"

instance HasField "rbrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbrr"

instance HasField "xzxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzxy"

instance HasField "rbrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbrg"

instance HasField "xzxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzxz"

instance HasField "rbrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbrb"

instance (FourElements n ~ True) => HasField "xzxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzxw"

instance (FourElements n ~ True) => HasField "rbra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbra"

instance HasField "xzyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzyx"

instance HasField "rbgr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbgr"

instance HasField "xzyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzyy"

instance HasField "rbgg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbgg"

instance HasField "xzyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzyz"

instance HasField "rbgb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbgb"

instance (FourElements n ~ True) => HasField "xzyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzyw"

instance (FourElements n ~ True) => HasField "rbga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbga"

instance HasField "xzzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzzx"

instance HasField "rbbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbbr"

instance HasField "xzzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzzy"

instance HasField "rbbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbbg"

instance HasField "xzzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzzz"

instance HasField "rbbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbbb"

instance (FourElements n ~ True) => HasField "xzzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzzw"

instance (FourElements n ~ True) => HasField "rbba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbba"

instance (FourElements n ~ True) => HasField "xzwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzwx"

instance (FourElements n ~ True) => HasField "rbar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbar"

instance (FourElements n ~ True) => HasField "xzwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzwy"

instance (FourElements n ~ True) => HasField "rbag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbag"

instance (FourElements n ~ True) => HasField "xzwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzwz"

instance (FourElements n ~ True) => HasField "rbab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbab"

instance (FourElements n ~ True) => HasField "xzww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xzww"

instance (FourElements n ~ True) => HasField "rbaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rbaa"

instance (FourElements n ~ True) => HasField "xwxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwxx"

instance (FourElements n ~ True) => HasField "rarr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rarr"

instance (FourElements n ~ True) => HasField "xwxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwxy"

instance (FourElements n ~ True) => HasField "rarg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rarg"

instance (FourElements n ~ True) => HasField "xwxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwxz"

instance (FourElements n ~ True) => HasField "rarb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rarb"

instance (FourElements n ~ True) => HasField "xwxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwxw"

instance (FourElements n ~ True) => HasField "rara" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rara"

instance (FourElements n ~ True) => HasField "xwyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwyx"

instance (FourElements n ~ True) => HasField "ragr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ragr"

instance (FourElements n ~ True) => HasField "xwyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwyy"

instance (FourElements n ~ True) => HasField "ragg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ragg"

instance (FourElements n ~ True) => HasField "xwyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwyz"

instance (FourElements n ~ True) => HasField "ragb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ragb"

instance (FourElements n ~ True) => HasField "xwyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwyw"

instance (FourElements n ~ True) => HasField "raga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "raga"

instance (FourElements n ~ True) => HasField "xwzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwzx"

instance (FourElements n ~ True) => HasField "rabr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rabr"

instance (FourElements n ~ True) => HasField "xwzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwzy"

instance (FourElements n ~ True) => HasField "rabg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rabg"

instance (FourElements n ~ True) => HasField "xwzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwzz"

instance (FourElements n ~ True) => HasField "rabb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "rabb"

instance (FourElements n ~ True) => HasField "xwzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwzw"

instance (FourElements n ~ True) => HasField "raba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "raba"

instance (FourElements n ~ True) => HasField "xwwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwwx"

instance (FourElements n ~ True) => HasField "raar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "raar"

instance (FourElements n ~ True) => HasField "xwwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwwy"

instance (FourElements n ~ True) => HasField "raag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "raag"

instance (FourElements n ~ True) => HasField "xwwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwwz"

instance (FourElements n ~ True) => HasField "raab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "raab"

instance (FourElements n ~ True) => HasField "xwww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "xwww"

instance (FourElements n ~ True) => HasField "raaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "raaa"

instance HasField "yxxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxxx"

instance HasField "grrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grrr"

instance HasField "yxxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxxy"

instance HasField "grrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grrg"

instance HasField "yxxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxxz"

instance HasField "grrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grrb"

instance (FourElements n ~ True) => HasField "yxxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxxw"

instance (FourElements n ~ True) => HasField "grra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grra"

instance HasField "yxyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxyx"

instance HasField "grgr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grgr"

instance HasField "yxyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxyy"

instance HasField "grgg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grgg"

instance HasField "yxyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxyz"

instance HasField "grgb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grgb"

instance (FourElements n ~ True) => HasField "yxyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxyw"

instance (FourElements n ~ True) => HasField "grga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grga"

instance HasField "yxzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxzx"

instance HasField "grbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grbr"

instance HasField "yxzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxzy"

instance HasField "grbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grbg"

instance HasField "yxzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxzz"

instance HasField "grbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grbb"

instance (FourElements n ~ True) => HasField "yxzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxzw"

instance (FourElements n ~ True) => HasField "grba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grba"

instance (FourElements n ~ True) => HasField "yxwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxwx"

instance (FourElements n ~ True) => HasField "grar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grar"

instance (FourElements n ~ True) => HasField "yxwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxwy"

instance (FourElements n ~ True) => HasField "grag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grag"

instance (FourElements n ~ True) => HasField "yxwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxwz"

instance (FourElements n ~ True) => HasField "grab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "grab"

instance (FourElements n ~ True) => HasField "yxww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yxww"

instance (FourElements n ~ True) => HasField "graa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "graa"

instance HasField "yyxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyxx"

instance HasField "ggrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggrr"

instance HasField "yyxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyxy"

instance HasField "ggrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggrg"

instance HasField "yyxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyxz"

instance HasField "ggrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggrb"

instance (FourElements n ~ True) => HasField "yyxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyxw"

instance (FourElements n ~ True) => HasField "ggra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggra"

instance HasField "yyyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyyx"

instance HasField "gggr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gggr"

instance HasField "yyyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyyy"

instance HasField "gggg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gggg"

instance HasField "yyyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyyz"

instance HasField "gggb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gggb"

instance (FourElements n ~ True) => HasField "yyyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyyw"

instance (FourElements n ~ True) => HasField "ggga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggga"

instance HasField "yyzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyzx"

instance HasField "ggbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggbr"

instance HasField "yyzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyzy"

instance HasField "ggbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggbg"

instance HasField "yyzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyzz"

instance HasField "ggbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggbb"

instance (FourElements n ~ True) => HasField "yyzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyzw"

instance (FourElements n ~ True) => HasField "ggba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggba"

instance (FourElements n ~ True) => HasField "yywx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yywx"

instance (FourElements n ~ True) => HasField "ggar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggar"

instance (FourElements n ~ True) => HasField "yywy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yywy"

instance (FourElements n ~ True) => HasField "ggag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggag"

instance (FourElements n ~ True) => HasField "yywz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yywz"

instance (FourElements n ~ True) => HasField "ggab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggab"

instance (FourElements n ~ True) => HasField "yyww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yyww"

instance (FourElements n ~ True) => HasField "ggaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ggaa"

instance HasField "yzxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzxx"

instance HasField "gbrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbrr"

instance HasField "yzxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzxy"

instance HasField "gbrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbrg"

instance HasField "yzxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzxz"

instance HasField "gbrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbrb"

instance (FourElements n ~ True) => HasField "yzxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzxw"

instance (FourElements n ~ True) => HasField "gbra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbra"

instance HasField "yzyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzyx"

instance HasField "gbgr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbgr"

instance HasField "yzyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzyy"

instance HasField "gbgg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbgg"

instance HasField "yzyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzyz"

instance HasField "gbgb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbgb"

instance (FourElements n ~ True) => HasField "yzyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzyw"

instance (FourElements n ~ True) => HasField "gbga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbga"

instance HasField "yzzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzzx"

instance HasField "gbbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbbr"

instance HasField "yzzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzzy"

instance HasField "gbbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbbg"

instance HasField "yzzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzzz"

instance HasField "gbbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbbb"

instance (FourElements n ~ True) => HasField "yzzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzzw"

instance (FourElements n ~ True) => HasField "gbba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbba"

instance (FourElements n ~ True) => HasField "yzwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzwx"

instance (FourElements n ~ True) => HasField "gbar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbar"

instance (FourElements n ~ True) => HasField "yzwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzwy"

instance (FourElements n ~ True) => HasField "gbag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbag"

instance (FourElements n ~ True) => HasField "yzwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzwz"

instance (FourElements n ~ True) => HasField "gbab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbab"

instance (FourElements n ~ True) => HasField "yzww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "yzww"

instance (FourElements n ~ True) => HasField "gbaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gbaa"

instance (FourElements n ~ True) => HasField "ywxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywxx"

instance (FourElements n ~ True) => HasField "garr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "garr"

instance (FourElements n ~ True) => HasField "ywxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywxy"

instance (FourElements n ~ True) => HasField "garg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "garg"

instance (FourElements n ~ True) => HasField "ywxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywxz"

instance (FourElements n ~ True) => HasField "garb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "garb"

instance (FourElements n ~ True) => HasField "ywxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywxw"

instance (FourElements n ~ True) => HasField "gara" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gara"

instance (FourElements n ~ True) => HasField "ywyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywyx"

instance (FourElements n ~ True) => HasField "gagr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gagr"

instance (FourElements n ~ True) => HasField "ywyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywyy"

instance (FourElements n ~ True) => HasField "gagg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gagg"

instance (FourElements n ~ True) => HasField "ywyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywyz"

instance (FourElements n ~ True) => HasField "gagb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gagb"

instance (FourElements n ~ True) => HasField "ywyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywyw"

instance (FourElements n ~ True) => HasField "gaga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gaga"

instance (FourElements n ~ True) => HasField "ywzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywzx"

instance (FourElements n ~ True) => HasField "gabr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gabr"

instance (FourElements n ~ True) => HasField "ywzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywzy"

instance (FourElements n ~ True) => HasField "gabg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gabg"

instance (FourElements n ~ True) => HasField "ywzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywzz"

instance (FourElements n ~ True) => HasField "gabb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gabb"

instance (FourElements n ~ True) => HasField "ywzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywzw"

instance (FourElements n ~ True) => HasField "gaba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gaba"

instance (FourElements n ~ True) => HasField "ywwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywwx"

instance (FourElements n ~ True) => HasField "gaar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gaar"

instance (FourElements n ~ True) => HasField "ywwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywwy"

instance (FourElements n ~ True) => HasField "gaag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gaag"

instance (FourElements n ~ True) => HasField "ywwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywwz"

instance (FourElements n ~ True) => HasField "gaab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gaab"

instance (FourElements n ~ True) => HasField "ywww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "ywww"

instance (FourElements n ~ True) => HasField "gaaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "gaaa"

instance HasField "zxxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxxx"

instance HasField "brrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brrr"

instance HasField "zxxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxxy"

instance HasField "brrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brrg"

instance HasField "zxxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxxz"

instance HasField "brrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brrb"

instance (FourElements n ~ True) => HasField "zxxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxxw"

instance (FourElements n ~ True) => HasField "brra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brra"

instance HasField "zxyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxyx"

instance HasField "brgr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brgr"

instance HasField "zxyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxyy"

instance HasField "brgg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brgg"

instance HasField "zxyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxyz"

instance HasField "brgb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brgb"

instance (FourElements n ~ True) => HasField "zxyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxyw"

instance (FourElements n ~ True) => HasField "brga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brga"

instance HasField "zxzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxzx"

instance HasField "brbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brbr"

instance HasField "zxzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxzy"

instance HasField "brbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brbg"

instance HasField "zxzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxzz"

instance HasField "brbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brbb"

instance (FourElements n ~ True) => HasField "zxzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxzw"

instance (FourElements n ~ True) => HasField "brba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brba"

instance (FourElements n ~ True) => HasField "zxwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxwx"

instance (FourElements n ~ True) => HasField "brar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brar"

instance (FourElements n ~ True) => HasField "zxwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxwy"

instance (FourElements n ~ True) => HasField "brag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brag"

instance (FourElements n ~ True) => HasField "zxwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxwz"

instance (FourElements n ~ True) => HasField "brab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "brab"

instance (FourElements n ~ True) => HasField "zxww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zxww"

instance (FourElements n ~ True) => HasField "braa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "braa"

instance HasField "zyxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyxx"

instance HasField "bgrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgrr"

instance HasField "zyxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyxy"

instance HasField "bgrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgrg"

instance HasField "zyxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyxz"

instance HasField "bgrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgrb"

instance (FourElements n ~ True) => HasField "zyxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyxw"

instance (FourElements n ~ True) => HasField "bgra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgra"

instance HasField "zyyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyyx"

instance HasField "bggr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bggr"

instance HasField "zyyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyyy"

instance HasField "bggg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bggg"

instance HasField "zyyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyyz"

instance HasField "bggb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bggb"

instance (FourElements n ~ True) => HasField "zyyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyyw"

instance (FourElements n ~ True) => HasField "bgga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgga"

instance HasField "zyzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyzx"

instance HasField "bgbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgbr"

instance HasField "zyzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyzy"

instance HasField "bgbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgbg"

instance HasField "zyzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyzz"

instance HasField "bgbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgbb"

instance (FourElements n ~ True) => HasField "zyzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyzw"

instance (FourElements n ~ True) => HasField "bgba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgba"

instance (FourElements n ~ True) => HasField "zywx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zywx"

instance (FourElements n ~ True) => HasField "bgar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgar"

instance (FourElements n ~ True) => HasField "zywy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zywy"

instance (FourElements n ~ True) => HasField "bgag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgag"

instance (FourElements n ~ True) => HasField "zywz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zywz"

instance (FourElements n ~ True) => HasField "bgab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgab"

instance (FourElements n ~ True) => HasField "zyww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zyww"

instance (FourElements n ~ True) => HasField "bgaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bgaa"

instance HasField "zzxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzxx"

instance HasField "bbrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbrr"

instance HasField "zzxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzxy"

instance HasField "bbrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbrg"

instance HasField "zzxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzxz"

instance HasField "bbrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbrb"

instance (FourElements n ~ True) => HasField "zzxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzxw"

instance (FourElements n ~ True) => HasField "bbra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbra"

instance HasField "zzyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzyx"

instance HasField "bbgr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbgr"

instance HasField "zzyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzyy"

instance HasField "bbgg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbgg"

instance HasField "zzyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzyz"

instance HasField "bbgb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbgb"

instance (FourElements n ~ True) => HasField "zzyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzyw"

instance (FourElements n ~ True) => HasField "bbga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbga"

instance HasField "zzzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzzx"

instance HasField "bbbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbbr"

instance HasField "zzzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzzy"

instance HasField "bbbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbbg"

instance HasField "zzzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzzz"

instance HasField "bbbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbbb"

instance (FourElements n ~ True) => HasField "zzzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzzw"

instance (FourElements n ~ True) => HasField "bbba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbba"

instance (FourElements n ~ True) => HasField "zzwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzwx"

instance (FourElements n ~ True) => HasField "bbar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbar"

instance (FourElements n ~ True) => HasField "zzwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzwy"

instance (FourElements n ~ True) => HasField "bbag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbag"

instance (FourElements n ~ True) => HasField "zzwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzwz"

instance (FourElements n ~ True) => HasField "bbab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbab"

instance (FourElements n ~ True) => HasField "zzww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zzww"

instance (FourElements n ~ True) => HasField "bbaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bbaa"

instance (FourElements n ~ True) => HasField "zwxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwxx"

instance (FourElements n ~ True) => HasField "barr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "barr"

instance (FourElements n ~ True) => HasField "zwxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwxy"

instance (FourElements n ~ True) => HasField "barg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "barg"

instance (FourElements n ~ True) => HasField "zwxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwxz"

instance (FourElements n ~ True) => HasField "barb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "barb"

instance (FourElements n ~ True) => HasField "zwxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwxw"

instance (FourElements n ~ True) => HasField "bara" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bara"

instance (FourElements n ~ True) => HasField "zwyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwyx"

instance (FourElements n ~ True) => HasField "bagr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bagr"

instance (FourElements n ~ True) => HasField "zwyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwyy"

instance (FourElements n ~ True) => HasField "bagg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bagg"

instance (FourElements n ~ True) => HasField "zwyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwyz"

instance (FourElements n ~ True) => HasField "bagb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "bagb"

instance (FourElements n ~ True) => HasField "zwyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwyw"

instance (FourElements n ~ True) => HasField "baga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "baga"

instance (FourElements n ~ True) => HasField "zwzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwzx"

instance (FourElements n ~ True) => HasField "babr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "babr"

instance (FourElements n ~ True) => HasField "zwzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwzy"

instance (FourElements n ~ True) => HasField "babg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "babg"

instance (FourElements n ~ True) => HasField "zwzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwzz"

instance (FourElements n ~ True) => HasField "babb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "babb"

instance (FourElements n ~ True) => HasField "zwzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwzw"

instance (FourElements n ~ True) => HasField "baba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "baba"

instance (FourElements n ~ True) => HasField "zwwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwwx"

instance (FourElements n ~ True) => HasField "baar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "baar"

instance (FourElements n ~ True) => HasField "zwwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwwy"

instance (FourElements n ~ True) => HasField "baag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "baag"

instance (FourElements n ~ True) => HasField "zwwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwwz"

instance (FourElements n ~ True) => HasField "baab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "baab"

instance (FourElements n ~ True) => HasField "zwww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "zwww"

instance (FourElements n ~ True) => HasField "baaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "baaa"

instance (FourElements n ~ True) => HasField "wxxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxxx"

instance (FourElements n ~ True) => HasField "arrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arrr"

instance (FourElements n ~ True) => HasField "wxxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxxy"

instance (FourElements n ~ True) => HasField "arrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arrg"

instance (FourElements n ~ True) => HasField "wxxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxxz"

instance (FourElements n ~ True) => HasField "arrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arrb"

instance (FourElements n ~ True) => HasField "wxxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxxw"

instance (FourElements n ~ True) => HasField "arra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arra"

instance (FourElements n ~ True) => HasField "wxyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxyx"

instance (FourElements n ~ True) => HasField "argr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "argr"

instance (FourElements n ~ True) => HasField "wxyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxyy"

instance (FourElements n ~ True) => HasField "argg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "argg"

instance (FourElements n ~ True) => HasField "wxyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxyz"

instance (FourElements n ~ True) => HasField "argb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "argb"

instance (FourElements n ~ True) => HasField "wxyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxyw"

instance (FourElements n ~ True) => HasField "arga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arga"

instance (FourElements n ~ True) => HasField "wxzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxzx"

instance (FourElements n ~ True) => HasField "arbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arbr"

instance (FourElements n ~ True) => HasField "wxzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxzy"

instance (FourElements n ~ True) => HasField "arbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arbg"

instance (FourElements n ~ True) => HasField "wxzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxzz"

instance (FourElements n ~ True) => HasField "arbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arbb"

instance (FourElements n ~ True) => HasField "wxzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxzw"

instance (FourElements n ~ True) => HasField "arba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arba"

instance (FourElements n ~ True) => HasField "wxwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxwx"

instance (FourElements n ~ True) => HasField "arar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arar"

instance (FourElements n ~ True) => HasField "wxwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxwy"

instance (FourElements n ~ True) => HasField "arag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arag"

instance (FourElements n ~ True) => HasField "wxwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxwz"

instance (FourElements n ~ True) => HasField "arab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "arab"

instance (FourElements n ~ True) => HasField "wxww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wxww"

instance (FourElements n ~ True) => HasField "araa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "araa"

instance (FourElements n ~ True) => HasField "wyxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyxx"

instance (FourElements n ~ True) => HasField "agrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agrr"

instance (FourElements n ~ True) => HasField "wyxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyxy"

instance (FourElements n ~ True) => HasField "agrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agrg"

instance (FourElements n ~ True) => HasField "wyxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyxz"

instance (FourElements n ~ True) => HasField "agrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agrb"

instance (FourElements n ~ True) => HasField "wyxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyxw"

instance (FourElements n ~ True) => HasField "agra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agra"

instance (FourElements n ~ True) => HasField "wyyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyyx"

instance (FourElements n ~ True) => HasField "aggr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aggr"

instance (FourElements n ~ True) => HasField "wyyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyyy"

instance (FourElements n ~ True) => HasField "aggg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aggg"

instance (FourElements n ~ True) => HasField "wyyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyyz"

instance (FourElements n ~ True) => HasField "aggb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aggb"

instance (FourElements n ~ True) => HasField "wyyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyyw"

instance (FourElements n ~ True) => HasField "agga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agga"

instance (FourElements n ~ True) => HasField "wyzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyzx"

instance (FourElements n ~ True) => HasField "agbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agbr"

instance (FourElements n ~ True) => HasField "wyzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyzy"

instance (FourElements n ~ True) => HasField "agbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agbg"

instance (FourElements n ~ True) => HasField "wyzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyzz"

instance (FourElements n ~ True) => HasField "agbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agbb"

instance (FourElements n ~ True) => HasField "wyzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyzw"

instance (FourElements n ~ True) => HasField "agba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agba"

instance (FourElements n ~ True) => HasField "wywx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wywx"

instance (FourElements n ~ True) => HasField "agar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agar"

instance (FourElements n ~ True) => HasField "wywy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wywy"

instance (FourElements n ~ True) => HasField "agag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agag"

instance (FourElements n ~ True) => HasField "wywz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wywz"

instance (FourElements n ~ True) => HasField "agab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agab"

instance (FourElements n ~ True) => HasField "wyww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wyww"

instance (FourElements n ~ True) => HasField "agaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "agaa"

instance (FourElements n ~ True) => HasField "wzxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzxx"

instance (FourElements n ~ True) => HasField "abrr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abrr"

instance (FourElements n ~ True) => HasField "wzxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzxy"

instance (FourElements n ~ True) => HasField "abrg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abrg"

instance (FourElements n ~ True) => HasField "wzxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzxz"

instance (FourElements n ~ True) => HasField "abrb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abrb"

instance (FourElements n ~ True) => HasField "wzxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzxw"

instance (FourElements n ~ True) => HasField "abra" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abra"

instance (FourElements n ~ True) => HasField "wzyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzyx"

instance (FourElements n ~ True) => HasField "abgr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abgr"

instance (FourElements n ~ True) => HasField "wzyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzyy"

instance (FourElements n ~ True) => HasField "abgg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abgg"

instance (FourElements n ~ True) => HasField "wzyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzyz"

instance (FourElements n ~ True) => HasField "abgb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abgb"

instance (FourElements n ~ True) => HasField "wzyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzyw"

instance (FourElements n ~ True) => HasField "abga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abga"

instance (FourElements n ~ True) => HasField "wzzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzzx"

instance (FourElements n ~ True) => HasField "abbr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abbr"

instance (FourElements n ~ True) => HasField "wzzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzzy"

instance (FourElements n ~ True) => HasField "abbg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abbg"

instance (FourElements n ~ True) => HasField "wzzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzzz"

instance (FourElements n ~ True) => HasField "abbb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abbb"

instance (FourElements n ~ True) => HasField "wzzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzzw"

instance (FourElements n ~ True) => HasField "abba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abba"

instance (FourElements n ~ True) => HasField "wzwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzwx"

instance (FourElements n ~ True) => HasField "abar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abar"

instance (FourElements n ~ True) => HasField "wzwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzwy"

instance (FourElements n ~ True) => HasField "abag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abag"

instance (FourElements n ~ True) => HasField "wzwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzwz"

instance (FourElements n ~ True) => HasField "abab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abab"

instance (FourElements n ~ True) => HasField "wzww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wzww"

instance (FourElements n ~ True) => HasField "abaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "abaa"

instance (FourElements n ~ True) => HasField "wwxx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwxx"

instance (FourElements n ~ True) => HasField "aarr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aarr"

instance (FourElements n ~ True) => HasField "wwxy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwxy"

instance (FourElements n ~ True) => HasField "aarg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aarg"

instance (FourElements n ~ True) => HasField "wwxz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwxz"

instance (FourElements n ~ True) => HasField "aarb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aarb"

instance (FourElements n ~ True) => HasField "wwxw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwxw"

instance (FourElements n ~ True) => HasField "aara" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aara"

instance (FourElements n ~ True) => HasField "wwyx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwyx"

instance (FourElements n ~ True) => HasField "aagr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aagr"

instance (FourElements n ~ True) => HasField "wwyy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwyy"

instance (FourElements n ~ True) => HasField "aagg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aagg"

instance (FourElements n ~ True) => HasField "wwyz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwyz"

instance (FourElements n ~ True) => HasField "aagb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aagb"

instance (FourElements n ~ True) => HasField "wwyw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwyw"

instance (FourElements n ~ True) => HasField "aaga" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aaga"

instance (FourElements n ~ True) => HasField "wwzx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwzx"

instance (FourElements n ~ True) => HasField "aabr" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aabr"

instance (FourElements n ~ True) => HasField "wwzy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwzy"

instance (FourElements n ~ True) => HasField "aabg" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aabg"

instance (FourElements n ~ True) => HasField "wwzz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwzz"

instance (FourElements n ~ True) => HasField "aabb" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aabb"

instance (FourElements n ~ True) => HasField "wwzw" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwzw"

instance (FourElements n ~ True) => HasField "aaba" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aaba"

instance (FourElements n ~ True) => HasField "wwwx" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwwx"

instance (FourElements n ~ True) => HasField "aaar" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aaar"

instance (FourElements n ~ True) => HasField "wwwy" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwwy"

instance (FourElements n ~ True) => HasField "aaag" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aaag"

instance (FourElements n ~ True) => HasField "wwwz" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwwz"

instance (FourElements n ~ True) => HasField "aaab" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aaab"

instance (FourElements n ~ True) => HasField "wwww" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "wwww"

instance (FourElements n ~ True) => HasField "aaaa" (Expr (VectorType n a)) (Expr (VectorType VL4 a)) where
  getField v = AccessField v "aaaa"

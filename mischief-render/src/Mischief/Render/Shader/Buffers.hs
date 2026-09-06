{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.Render.Shader.Buffers where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Data
import Data.Foldable
import Data.Singletons (SingI)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as VS
import Foreign (Bits (shiftR), Ptr, Storable (peek, peekByteOff, peekElemOff, poke, pokeElemOff), Word32, Word8, free, mallocBytes, nullPtr, peekArray, pokeArray)
import GHC.Float (castFloatToWord32)
import GHC.Generics
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import Mischief.ECS (System)
import Mischief.Math.Vec qualified as Math
import Mischief.Render.Shader.Singletons
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.WGPU (withWGPUString)
import Mischief.WGPU.Opaque (WGPUBuffer)
import Mischief.WGPU.Types.Enums (wGPUBufferUsage_Uniform, wgpuFalse)
import Mischief.WGPU.Types.General qualified as TG

-- import Mischief.WGPU.Types.General (WGPUBufferDescriptor (..))

data Field' a = Field'

type family Field f a where
  Field GPU a = a
  Field CPU a = ToCPU a
  Field Internal a = Field' a

type family ToCPU a where
  ToCPU I32 = Int
  ToCPU F32 = Float
  ToCPU (Expr (VectorType VL2 a)) = Math.V2 (ToCPU (Expr (Primitive a)))
  ToCPU (Expr (VectorType VL3 a)) = Math.V3 (ToCPU (Expr (Primitive a)))
  ToCPU (Expr (VectorType VL4 a)) = Math.V4 (ToCPU (Expr (Primitive a)))
  ToCPU (Expr (MatrixType VL2 n)) = (ToCPU (Expr (VectorType n TFloat)), ToCPU (Expr (VectorType n TFloat)))
  ToCPU (Expr (MatrixType VL3 n)) = (ToCPU (Expr (VectorType n TFloat)), ToCPU (Expr (VectorType n TFloat)), ToCPU (Expr (VectorType n TFloat)))
  ToCPU (Expr (MatrixType VL4 n)) = (ToCPU (Expr (VectorType n TFloat)), ToCPU (Expr (VectorType n TFloat)), ToCPU (Expr (VectorType n TFloat)), ToCPU (Expr (VectorType n TFloat)))

type family AlignmentOf a where
  AlignmentOf I32 = 4
  AlignmentOf F32 = 4
  AlignmentOf (Expr (VectorType VL2 a)) = 8
  AlignmentOf (Expr (VectorType VL3 a)) = 16
  AlignmentOf (Expr (VectorType VL4 a)) = 16
  AlignmentOf (Expr (MatrixType n VL2)) = 8
  AlignmentOf (Expr (MatrixType n m)) = 16

type family SizeOf a where
  SizeOf I32 = 4
  SizeOf F32 = 4
  SizeOf (Expr (VectorType VL2 a)) = 8
  SizeOf (Expr (VectorType VL3 a)) = 12
  SizeOf (Expr (VectorType VL4 a)) = 16
  SizeOf Mat2x2 = 16
  SizeOf Mat3x2 = 24
  SizeOf Mat4x2 = 32
  SizeOf Mat3x3 = 48
  SizeOf Mat4x3 = 64
  SizeOf Mat2x4 = 32
  SizeOf Mat3x4 = 48
  SizeOf Mat4x4 = 64

newtype Size = Size Nat deriving newtype (Show, Num, Eq, Ord)

newtype Alignment = Alignment Nat deriving newtype (Show, Num, Eq, Ord)

newtype Offset = Offset Nat deriving newtype (Show, Num, Eq, Ord)

newtype BufferStruct = BufferStruct [(Text, Text)]

class Bufferable a where
  dummyBuffer :: Integer -> a GPU
  default dummyBuffer :: (Generic (a GPU), GDummyBuffer (Rep (a GPU))) => Integer -> a GPU
  dummyBuffer i =
    let (x, _) = gDummyBuffer @(Rep (a GPU)) i 0
     in to x

  getFields :: [(Text, Text)]
  default getFields :: (GGetFields (Rep (a GPU))) => [(Text, Text)]
  getFields =
    let (t, Offset offset) = gGetFields @(Rep (a GPU)) 0
        t' = roundTo16Pad offset
     in t ++ t'

  getSize :: Int
  default getSize :: (GGetFields (Rep (a GPU))) => Int
  getSize =
    let Offset offset = snd $ gGetFields @(Rep (a GPU)) 0
     in fromIntegral $ roundTo16 offset

  putBytes :: a CPU -> Ptr Word8 -> IO Int
  default putBytes :: (GGetBytes (Rep (a CPU)) (Rep (a GPU)), Generic (a CPU)) => a CPU -> Ptr Word8 -> IO Int
  putBytes a ptr = do
    Offset x <- gGetBytes @(Rep (a CPU)) @(Rep (a GPU)) (from a) ptr 0
    x' <- roundTo16Bytes ptr x
    pure $ fromIntegral x'

roundTo16 :: Nat -> Nat
roundTo16 x | x `mod` 16 == 0 = x
roundTo16 x = roundTo16 (x + 4)

roundTo16Pad :: Nat -> [(Text, Text)]
roundTo16Pad x | x `mod` 16 == 0 = []
roundTo16Pad x = ("p" <> T.pack (show x), "f32") : roundTo16Pad (x + 4)

roundTo16Bytes :: Ptr Word8 -> Nat -> IO Nat
roundTo16Bytes _ x | x `mod` 16 == 0 = pure x
roundTo16Bytes ptr x = do
  pokeElemOff ptr (fromIntegral x) 0
  pokeElemOff ptr (fromIntegral x + 1) 0
  pokeElemOff ptr (fromIntegral x + 2) 0
  pokeElemOff ptr (fromIntegral x + 3) 0
  roundTo16Bytes ptr (x + 4)

addPadding :: Offset -> Alignment -> [(Text, Text)]
addPadding (Offset x) (Alignment y) | x `mod` y == 0 = []
addPadding x y = ("p" <> T.pack (show x), "f32") : addPadding (x + 4) y

addPaddingBytes :: Offset -> Alignment -> Ptr Word8 -> IO Offset
addPaddingBytes (Offset x) (Alignment y) _ | x `mod` y == 0 = pure (Offset x)
addPaddingBytes (Offset x) y ptr = do
  pokeElemOff ptr (fromIntegral x + 0) 0
  pokeElemOff ptr (fromIntegral x + 1) 0
  pokeElemOff ptr (fromIntegral x + 2) 0
  pokeElemOff ptr (fromIntegral x + 3) 0
  addPaddingBytes (Offset $ x + 4) y ptr

class GGetFields f where
  gGetFields :: Offset -> ([(Text, Text)], Offset)

instance GGetFields V1 where
  gGetFields _ = undefined

instance GGetFields U1 where
  gGetFields _ = ([], 0)

instance (GGetFields f, GGetFields g) => GGetFields (f :*: g) where
  gGetFields offset =
    let (t, offset') = gGetFields @f offset
        (t2, offset'') = gGetFields @g $ offset'
     in (t <> t2, offset'')

instance {-# OVERLAPPING #-} (SingI a, KnownNat size, KnownNat alignment, SizeOf (Expr a) ~ size, AlignmentOf (Expr a) ~ alignment) => GGetFields (S1 u (K1 i (Expr a))) where
  gGetFields (Offset offset) =
    let alignment = fromIntegral $ natVal (Proxy @alignment)
        size = fromIntegral $ natVal (Proxy @size)
        t = addPadding (Offset offset) (Alignment alignment)
     in (t <> [("q" <> T.pack (show $ offset + fromIntegral (length t) * 4), typeToWGSL (undefined :: Expr a))], Offset $ offset + size + fromIntegral (length t) * 4)

instance (GGetFields (Rep f)) => GGetFields (S1 a (K1 x f)) where
  gGetFields = gGetFields @(Rep f)

instance (GGetFields c) => GGetFields (C1 i c) where
  gGetFields = gGetFields @c

instance (GGetFields f) => GGetFields (D1 a f) where
  gGetFields = gGetFields @f

class GetBytes a where
  getBytes :: ToCPU a -> [Word8]

class GGetBytes f g where
  gGetBytes :: f p -> Ptr Word8 -> Offset -> IO Offset

instance GGetBytes V1 V1 where
  gGetBytes _ _ _ = undefined

instance GGetBytes U1 U1 where
  gGetBytes _ _ _ = pure 0

instance (GGetBytes f1 g1, GGetBytes f2 g2) => GGetBytes (f1 :*: f2) (g1 :*: g2) where
  gGetBytes (a :*: b) ptr offset = do
    offset' <- gGetBytes @f1 @g1 a ptr offset
    gGetBytes @f2 @g2 b ptr offset'

instance {-# OVERLAPPING #-} (GetBytes (Expr b), KnownNat alignment, SizeOf (Expr b) ~ size, AlignmentOf (Expr b) ~ alignment, a ~ ToCPU (Expr b)) => GGetBytes (S1 u (K1 i a)) (S1 u (K1 i (Expr b))) where
  gGetBytes (M1 (K1 a)) ptr offset = do
    let alignment = fromIntegral $ natVal (Proxy @alignment)
    offset' <- addPaddingBytes offset alignment ptr
    let bytes = getBytes @(Expr b) a
    addBytes bytes ptr offset'

addBytes :: [Word8] -> Ptr Word8 -> Offset -> IO Offset
addBytes [] _ x = pure x
addBytes (w : ws) ptr (Offset x) = do
  pokeElemOff ptr (fromIntegral x) w
  addBytes ws ptr (Offset $ x + 1)

-- (t <> [("var", typeToWGSL (undefined :: Expr a))], Offset $ offset + size + fromIntegral (length t) * 4)

instance (GGetBytes (Rep f) (Rep g), Generic f) => GGetBytes (S1 a (K1 x f)) (S1 a' (K1 x' g)) where
  gGetBytes (M1 (K1 a)) = gGetBytes @(Rep f) @(Rep g) (from a)

instance (GGetBytes f g) => GGetBytes (C1 i f) (C1 i' g) where
  gGetBytes (M1 a) = gGetBytes @f @g a

instance (GGetBytes f g) => GGetBytes (D1 a f) (D1 a' g) where
  gGetBytes (M1 a) = gGetBytes @f @g a

word32ToWords8 :: Word32 -> [Word8]
-- word32ToWords8 x = [fromIntegral (x `shiftR` 24), fromIntegral (x `shiftR` 16), fromIntegral (x `shiftR` 8), fromIntegral x]
word32ToWords8 x = [fromIntegral x, fromIntegral (x `shiftR` 8), fromIntegral (x `shiftR` 16), fromIntegral (x `shiftR` 24)]

instance GetBytes I32 where
  getBytes a = word32ToWords8 (fromIntegral a)

instance GetBytes F32 where
  getBytes a = word32ToWords8 (castFloatToWord32 a)

instance (GetBytes (Expr (Primitive b))) => GetBytes (Expr (VectorType VL2 b)) where
  getBytes (Math.V2 a b) = getBytes @(Expr (Primitive b)) a ++ getBytes @(Expr (Primitive b)) b

instance (GetBytes (Expr (Primitive b))) => GetBytes (Expr (VectorType VL3 b)) where
  getBytes (Math.V3 a b c) = getBytes @(Expr (Primitive b)) a ++ getBytes @(Expr (Primitive b)) b ++ getBytes @(Expr (Primitive b)) c

instance (GetBytes (Expr (Primitive b))) => GetBytes (Expr (VectorType VL4 b)) where
  getBytes (Math.V4 a b c d) = getBytes @(Expr (Primitive b)) a ++ getBytes @(Expr (Primitive b)) b ++ getBytes @(Expr (Primitive b)) c ++ getBytes @(Expr (Primitive b)) d

instance (GetBytes (Expr (VectorType a TFloat)), ToCPU (Expr (MatrixType VL2 a)) ~ (v0, v0)) => GetBytes (Expr (MatrixType VL2 a)) where
  getBytes (a, b) = getBytes @(Expr (VectorType a TFloat)) a ++ getBytes @(Expr (VectorType a TFloat)) b

instance (GetBytes (Expr (VectorType a TFloat)), ToCPU (Expr (MatrixType VL3 a)) ~ (v0, v0, v0)) => GetBytes (Expr (MatrixType VL3 a)) where
  getBytes (a, b, c) = getBytes @(Expr (VectorType a TFloat)) a ++ getBytes @(Expr (VectorType a TFloat)) b ++ getBytes @(Expr (VectorType a TFloat)) c

instance (GetBytes (Expr (VectorType a TFloat)), ToCPU (Expr (MatrixType VL4 a)) ~ (v0, v0, v0, v0)) => GetBytes (Expr (MatrixType VL4 a)) where
  getBytes (a, b, c, d) = getBytes @(Expr (VectorType a TFloat)) a ++ getBytes @(Expr (VectorType a TFloat)) b ++ getBytes @(Expr (VectorType a TFloat)) c ++ getBytes @(Expr (VectorType a TFloat)) d

class GDummyBuffer f where
  gDummyBuffer :: Integer -> Offset -> (f p, Offset)

instance GDummyBuffer V1 where
  gDummyBuffer = undefined

instance GDummyBuffer U1 where
  gDummyBuffer _ _ = (U1, 0)

instance (GDummyBuffer f, GDummyBuffer g) => GDummyBuffer (f :*: g) where
  gDummyBuffer index offset = do
    let (f, offset') = gDummyBuffer @f index offset
    let (g, offset'') = gDummyBuffer @g index offset'
    (f :*: g, offset'')

instance (GDummyBuffer f) => GDummyBuffer (D1 (MetaData s a b c) f) where
  gDummyBuffer index offset = do
    let (f, offset') = gDummyBuffer @f index offset
    (M1 f, offset')

instance {-# OVERLAPPING #-} (KnownNat size, KnownNat alignment, SizeOf (Expr a) ~ size, AlignmentOf (Expr a) ~ alignment) => GDummyBuffer (S1 b (K1 x (Expr a))) where
  gDummyBuffer index offset = do
    let alignment = fromIntegral $ natVal (Proxy @alignment)
    let size = natVal (Proxy @size)
    let t = addPadding offset (Alignment alignment)
    let offset' = offset + Offset (fromIntegral $ length t) * 4
    (M1 $ K1 $ VarCustom $ "b" <> T.pack (show index) <> ".q" <> T.pack (show offset'), offset' + Offset (fromIntegral size))

instance {-# OVERLAPPABLE #-} (GDummyBuffer (Rep f), Generic f) => GDummyBuffer (S1 b (K1 x f)) where
  gDummyBuffer x y =
    let (a, o) = gDummyBuffer @(Rep f) x y
     in (M1 $ K1 $ to a, o)

instance (GDummyBuffer c) => GDummyBuffer (C1 i c) where
  gDummyBuffer x y =
    let (a, o) = gDummyBuffer @c x y
     in (M1 a, o)

data Test f = Test
  { field1 :: Field f Vec3i,
    field2 :: Field f I32,
    fieldM :: Field f Mat2x2,
    test2 :: Test2 f
  }
  deriving (Generic, Bufferable)

data Test2 f = Test2
  { field3 :: Field f Vec3i,
    field4 :: Field f I32,
    field5 :: Field f I32
  }
  deriving (Generic, Bufferable)

-- test2 :: IO ()
-- test2 = do
--   let x :: Test CPU =
--         Test
--           { field1 = Math.V3 5 5 5,
--             field2 = 10,
--             test2 =
--               Test2
--                 { field3 = Math.V3 6 6 6,
--                   field4 = 30,
--                   field5 = 20
--                 }
--           }

--   let s = getSize @Test
--   ptr <- mallocBytes s

--   print s
--   x <- putBytes x ptr
--   print x

--   l <- for [0 .. s - 1] $ \i -> peekElemOff ptr i
--   print l

--   let y = getFields @Test
--   print y

--   free ptr

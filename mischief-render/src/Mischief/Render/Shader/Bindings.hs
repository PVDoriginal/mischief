{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.Render.Shader.Bindings where

import Data.Data
import Data.Default (Default (def))
import Data.Kind
import Data.Primitive.Ptr
import Data.Singletons (SingI)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Foreign (with)
import Foreign.C.ConstPtr
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), U1 (U1), V1 (..), to, (:*:) ((:*:)))
import GHC.TypeLits
import Mischief.ECS (System)
import Mischief.Render.Core
import Mischief.Render.Shader.Singletons
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Texture
import Mischief.WGPU
import Mischief.WGPU.Opaque (WGPUBindGroup, WGPUBindGroupLayout)
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

-- data Binding (index :: Nat) a where
--   Binding :: forall b index. Expr (AssociatedExpr b) -> Binding index b
--   BindEntry :: WGPUBindGroupEntry -> Binding index b

-- data Uniform (index :: Nat) a where
--   Uniform :: forall b index. WGPUBindGroupEntry -> Uniform index b

newtype UniformBinding (n :: Nat) a = UniformBinding a

type family DesugarExpr a where
  DesugarExpr (Expr a) = a

type family Uniform f (n :: Nat) a where
  Uniform Internal n b = UniformBinding n b
  Uniform CPU n Sampler = Sampler
  Uniform GPU n Sampler = Expr TSampler
  Uniform CPU n Texture = TextureView
  Uniform GPU n Texture = Expr TTexture
  Uniform GPU n (Expr b) = Expr b

-- type family ToGpu a where
--   T

type family AssociatedExpr a where
  AssociatedExpr (Expr a) = a
  AssociatedExpr Texture = TTexture
  AssociatedExpr Sampler = TSampler

-- instance (KnownNat n, SingI (AssociatedExpr a)) => Default (Binding n a) where
--   def = Binding $ BindingVar (natVal (Proxy @n))

data BindingData = BindingData
  { bType :: Text,
    index :: Integer
  }
  deriving (Show)

newtype Bindings = Bindings [BindingData] deriving newtype (Semigroup, Show)

class Bindable a where
  collectBindings :: Proxy (a Internal) -> Bindings
  default collectBindings :: (GCollectBindings (Rep (a Internal))) => Proxy (a Internal) -> Bindings
  collectBindings _ = gCollectBindings (Proxy @(Rep (a Internal)))

  collectLayouts :: Proxy (a Internal) -> [WGPUBindGroupLayoutEntry]
  default collectLayouts :: (GCollectBindings (Rep (a Internal))) => Proxy (a Internal) -> [WGPUBindGroupLayoutEntry]
  collectLayouts _ = gCollectLayouts (Proxy @(Rep (a Internal)))

  collectEntries :: a CPU -> [WGPUBindGroupEntry]
  default collectEntries :: (GCollectEntries (Rep (a CPU)) (Rep (a Internal)), Generic (a CPU)) => a CPU -> [WGPUBindGroupEntry]
  collectEntries a = gCollectEntries @(Rep (a CPU)) @(Rep (a Internal)) (from a)

  dummyBinding :: a GPU
  default dummyBinding :: (Generic (a GPU), GDummyBinding (Rep (a Internal)) (Rep (a GPU))) => a GPU
  dummyBinding = to $ gDummyBinding @(Rep (a Internal)) @(Rep (a GPU))

-- dummyBinding

class CollectBindings a where
  collectBinding :: Proxy a -> BindingData
  collectLayout :: Proxy a -> WGPUBindGroupLayoutEntry

class GCollectBindings f where
  gCollectBindings :: Proxy f -> Bindings
  gCollectLayouts :: Proxy f -> [WGPUBindGroupLayoutEntry]

instance GCollectBindings V1 where
  gCollectBindings _ = Bindings []
  gCollectLayouts _ = []

instance GCollectBindings U1 where
  gCollectBindings _ = Bindings []
  gCollectLayouts _ = []

instance (GCollectBindings f, GCollectBindings g) => GCollectBindings (f :*: g) where
  gCollectBindings _ = gCollectBindings (Proxy @f) <> gCollectBindings (Proxy @g)
  gCollectLayouts _ = gCollectLayouts (Proxy @f) <> gCollectLayouts (Proxy @g)

instance (CollectBindings c) => GCollectBindings (K1 i c) where
  gCollectBindings _ = Bindings [collectBinding (Proxy @c)]
  gCollectLayouts _ = [collectLayout (Proxy @c)]

instance (GCollectBindings f) => GCollectBindings (M1 i t f) where
  gCollectBindings _ = gCollectBindings (Proxy @f)
  gCollectLayouts _ = gCollectLayouts (Proxy @f)

instance (KnownNat n) => CollectBindings (UniformBinding n Sampler) where
  collectBinding _ = BindingData {bType = T.pack "sampler", index = natVal (Proxy @n)}
  collectLayout _ =
    WGPUBindGroupLayoutEntry
      { nextInChain = nullPtr,
        binding = fromInteger (natVal (Proxy @n)),
        visibility = wGPUShaderStage_Fragment,
        bindingArraySize = 0,
        buffer = unusedBufferLayout,
        storageTexture = unusedStorageTextureLayout,
        texture = unusedTextureLayout,
        sampler =
          WGPUSamplerBindingLayout
            { _type = wGPUSamplerBindingType_Filtering,
              nextInChain = nullPtr
            }
      }

instance (KnownNat n) => CollectBindings (UniformBinding n Texture) where
  collectBinding _ = BindingData {bType = T.pack "texture_2d<f32>", index = natVal (Proxy @n)}
  collectLayout _ =
    WGPUBindGroupLayoutEntry
      { nextInChain = nullPtr,
        binding = fromInteger (natVal (Proxy @n)),
        visibility = wGPUShaderStage_Fragment,
        bindingArraySize = 0,
        buffer = unusedBufferLayout,
        sampler = unusedSamplerLayout,
        storageTexture = unusedStorageTextureLayout,
        texture =
          WGPUTextureBindingLayout
            { sampleType = wGPUTextureSampleType_Float,
              multisampled = wgpuFalse,
              viewDimension = wGPUTextureViewDimension_2D,
              nextInChain = nullPtr
            }
      }

class CollectEntry a n where
  collectEntry :: a -> WGPUBindGroupEntry

class GCollectEntries f g where
  gCollectEntries :: f p -> [WGPUBindGroupEntry]

instance GCollectEntries V1 V1 where
  gCollectEntries _ = []

instance GCollectEntries U1 U1 where
  gCollectEntries _ = []

instance (GCollectEntries f1 g1, GCollectEntries f2 g2) => GCollectEntries (f1 :*: f2) (g1 :*: g2) where
  gCollectEntries (f :*: g) = gCollectEntries @f1 @g1 f <> gCollectEntries @f2 @g2 g

instance (CollectEntry c n) => GCollectEntries (K1 i c) (K1 i n) where
  gCollectEntries (K1 a) = [collectEntry @c @n a]

instance (GCollectEntries f n) => GCollectEntries (M1 i t f) (M1 i t n) where
  gCollectEntries (M1 a) = gCollectEntries @f @n a

instance (KnownNat n) => CollectEntry TextureView (UniformBinding n b) where
  collectEntry (TextureView textureView) =
    WGPUBindGroupEntry
      { binding = fromInteger $ natVal $ Proxy @n,
        textureView,
        size = 0,
        offset = 0,
        sampler = nullPtr,
        buffer = nullPtr,
        nextInChain = nullPtr
      }

instance (KnownNat n) => CollectEntry Sampler (UniformBinding n b) where
  collectEntry (Sampler sampler) =
    WGPUBindGroupEntry
      { binding = fromInteger $ natVal $ Proxy @n,
        textureView = nullPtr,
        size = 0,
        offset = 0,
        sampler,
        buffer = nullPtr,
        nextInChain = nullPtr
      }

class DummyBinding a b where
  dummyBinding' :: b

class GDummyBinding f g where
  gDummyBinding :: g p

instance GDummyBinding V1 V1 where
  gDummyBinding = undefined

instance GDummyBinding U1 U1 where
  gDummyBinding = U1

instance (GDummyBinding f1 g1, GDummyBinding f2 g2) => GDummyBinding (f1 :*: f2) (g1 :*: g2) where
  gDummyBinding = gDummyBinding @f1 @g1 :*: gDummyBinding @f2 @g2

instance (DummyBinding a b) => GDummyBinding (K1 i a) (K1 i b) where
  gDummyBinding = K1 $ dummyBinding' @a @b

instance (GDummyBinding a b) => GDummyBinding (M1 i t a) (M1 i t b) where
  gDummyBinding = M1 $ gDummyBinding @a @b

instance (KnownNat n, SingI b) => DummyBinding (UniformBinding n a) (Expr b) where
  dummyBinding' = BindingVar (natVal (Proxy @n))

-- instance (KnownNat n) => Bindable (UniformBinding n Sampler) where
--   collectBindings _ = Bindings [BindingData {bType = T.pack "sampler", index = natVal (Proxy @n)}]
--   collectLayouts _ =
--     [ WGPUBindGroupLayoutEntry
--         { nextInChain = nullPtr,
--           binding = fromInteger (natVal (Proxy @n)),
--           visibility = wGPUShaderStage_Fragment,
--           bindingArraySize = 0,
--           buffer = unusedBufferLayout,
--           storageTexture = unusedStorageTextureLayout,
--           texture = unusedTextureLayout,
--           sampler =
--             WGPUSamplerBindingLayout
--               { _type = wGPUSamplerBindingType_Filtering,
--                 nextInChain = nullPtr
--               }
--         }
--     ]

--   collectEntries (UniformBinding a) = [bind a]

-- -- instance (KnownNat n, SingI b, f Internal ~ UniformBinding n a', f GPU ~ Expr b) => ToDummy f where
-- --   dummyB = BindingVar (natVal (Proxy @n))

-- instance (KnownNat n) => Bindable (UniformBinding n Texture) where
--   collectBindings _ = Bindings [BindingData {bType = T.pack "texture_2d<f32>", index = natVal (Proxy @n)}]
--   collectLayouts _ =
--     [ WGPUBindGroupLayoutEntry
--         { nextInChain = nullPtr,
--           binding = fromInteger (natVal (Proxy @n)),
--           visibility = wGPUShaderStage_Fragment,
--           bindingArraySize = 0,
--           buffer = unusedBufferLayout,
--           sampler = unusedSamplerLayout,
--           storageTexture = unusedStorageTextureLayout,
--           texture =
--             WGPUTextureBindingLayout
--               { sampleType = wGPUTextureSampleType_Float,
--                 multisampled = wgpuFalse,
--                 viewDimension = wGPUTextureViewDimension_2D,
--                 nextInChain = nullPtr
--               }
--         }
--     ]

--   collectEntries (UniformBinding a) = [bind a]

-- -- dummyB = Binding $ BindingVar (natVal (Proxy @n))

-- instance Bindable ()

-- instance (KnownNat n, SingI (Primitive a)) => Bindable (UniformBinding n (Expr (Primitive a))) where
--   collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (Primitive a)), index = natVal (Proxy @n)}]
--   collectLayouts _ = undefined

--   collectEntries (UniformBinding a) = [bind a]

-- -- dummyB = Binding $ BindingVar (natVal (Proxy @n))

-- instance (KnownNat n, SingI (VectorType m a)) => Bindable (UniformBinding n (Expr (VectorType m a))) where
--   collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (VectorType m a)), index = natVal (Proxy @n)}]
--   collectLayouts _ = undefined

--   collectEntries (UniformBinding a) = [bind a]

-- -- dummyB = Binding $ BindingVar (natVal (Proxy @n))

-- instance (KnownNat n, SingI (ArrayType m a)) => Bindable (UniformBinding n (Expr (ArrayType m a))) where
--   collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (ArrayType m a)), index = natVal (Proxy @n)}]
--   collectLayouts _ = undefined

--   collectEntries (UniformBinding a) = [bind a]

-- -- dummyB = Binding $ BindingVar (natVal (Proxy @n))

createBindLayout :: forall b. (Bindable b) => RenderDevice -> IO BindLayout
createBindLayout (RenderDevice device) = do
  let layouts = VS.fromList $ collectLayouts (Proxy @(b Internal))

  withWGPUString "bind group layout" $ \label -> do
    x <- VS.unsafeWith layouts $ \entries -> do
      let desc =
            WGPUBindGroupLayoutDescriptor
              { entryCount = fromIntegral $ VS.length layouts,
                entries = ConstPtr entries,
                label,
                nextInChain = nullPtr
              }
      with desc $ \desc -> wgpuDeviceCreateBindGroupLayout device (ConstPtr desc)

    pure (BindLayout x)

createBindGroup :: forall b. (Bindable b) => RenderDevice -> BindLayout -> b CPU -> IO (Ptr WGPUBindGroup)
createBindGroup (RenderDevice device) (BindLayout bindGroupLayout) b = do
  let entries = VS.fromList $ collectEntries b

  withWGPUString "bind group" $ \label -> do
    VS.unsafeWith entries $ \entries' -> do
      let desc =
            WGPUBindGroupDescriptor
              { layout = bindGroupLayout,
                entryCount = fromIntegral $ VS.length entries,
                label = label,
                nextInChain = nullPtr,
                entries = ConstPtr entries'
              }

      with desc $ \desc -> wgpuDeviceCreateBindGroup device (ConstPtr desc)

-- class (Bindable link) => ConvertBindings link where
--   type CpuBind link
--   type GpuBind link
--   toLink :: CpuBind link -> link
--   mkGpu :: GpuBind link

-- bind :: a -> WGPUBindGroupEntry
-- bind = undefined

-- instance (KnownNat n) => ConvertBindings (UniformBinding (n :: Nat) Texture) where
--   type CpuBind (UniformBinding (n :: Nat) Texture) = TextureView
--   type GpuBind (UniformBinding (n :: Nat) Texture) = Expr TTexture
--   toLink (TextureView view) =
--     Uniform
--       WGPUBindGroupEntry
--         { binding = fromInteger (natVal (Proxy @n)),
--           textureView = view,
--           size = 0,
--           offset = 0,
--           sampler = nullPtr,
--           buffer = nullPtr,
--           nextInChain = nullPtr
--         }

--   mkGpu = BindingVar (natVal (Proxy @n))

-- instance (KnownNat n) => ConvertBindings (Uniform (n :: Nat) Sampler) where
--   type CpuBind (Uniform (n :: Nat) Sampler) = Sampler
--   type GpuBind (Uniform (n :: Nat) Sampler) = Expr TSampler
--   toLink (Sampler sampler) =
--     Uniform
--       WGPUBindGroupEntry
--         { binding = fromInteger (natVal (Proxy @n)),
--           textureView = nullPtr,
--           size = 0,
--           offset = 0,
--           sampler = sampler,
--           buffer = nullPtr,
--           nextInChain = nullPtr
--         }

--   mkGpu = BindingVar (natVal (Proxy @n))

-- class GMkDummy i o where
--   gMkDummy :: o

-- instance GMkDummy (K1 a b c) (K1 a b c) where

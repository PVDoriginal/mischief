{-# LANGUAGE AllowAmbiguousTypes #-}

module Mischief.Render.Shader.Bindings where

import Data.Data
import Data.Default (Default (def))
import Data.Primitive.Ptr
import Data.Singletons (SingI)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Storable qualified as VS
import Foreign (with)
import Foreign.C.ConstPtr
import GHC.Generics (Generic (Rep, from), K1 (K1), M1 (M1), U1 (U1), V1, (:*:) ((:*:)))
import GHC.TypeLits
import Mischief.ECS (System)
import Mischief.Render.Core
import Mischief.Render.Shader.Singletons
import Mischief.Render.Shader.State
import Mischief.Render.Shader.Types
import Mischief.Render.Texture
import Mischief.WGPU
import Mischief.WGPU.Opaque (WGPUBindGroup)
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

data Binding (index :: Nat) a where
  Binding :: forall b index. Expr (AssociatedExpr b) -> Binding index b
  BindEntry :: WGPUBindGroupEntry -> Binding index b

type family AssociatedExpr a where
  AssociatedExpr (Expr a) = a
  AssociatedExpr Texture = TTexture
  AssociatedExpr Sampler = TSampler

instance (KnownNat n, SingI (AssociatedExpr a)) => Default (Binding n a) where
  def = Binding $ BindingVar (natVal (Proxy @n))

data BindingData = BindingData
  { bType :: Text,
    index :: Integer
  }
  deriving (Show)

newtype Bindings = Bindings [BindingData] deriving newtype (Semigroup, Show)

class (Default a) => Bindable a where
  collectBindings :: Proxy a -> Bindings
  default collectBindings :: (Bindable' (Rep a)) => Proxy a -> Bindings
  collectBindings _ = collectBindings' (Proxy @(Rep a))

  collectLayouts :: Proxy a -> [WGPUBindGroupLayoutEntry]
  default collectLayouts :: (Bindable' (Rep a)) => Proxy a -> [WGPUBindGroupLayoutEntry]
  collectLayouts _ = collectLayouts' (Proxy @(Rep a))

  collectEntries :: a -> [WGPUBindGroupEntry]
  default collectEntries :: (Bindable' (Rep a), Generic a) => a -> [WGPUBindGroupEntry]
  collectEntries a = collectEntries' (from a)

class Bindable' f where
  collectBindings' :: Proxy f -> Bindings
  collectLayouts' :: Proxy f -> [WGPUBindGroupLayoutEntry]
  collectEntries' :: f p -> [WGPUBindGroupEntry]

instance Bindable' V1 where
  collectBindings' _ = Bindings []
  collectLayouts' _ = []
  collectEntries' _ = []

instance Bindable' U1 where
  collectBindings' _ = Bindings []
  collectLayouts' _ = []
  collectEntries' _ = []

instance (Bindable' f, Bindable' g) => Bindable' (f :*: g) where
  collectBindings' _ = collectBindings' (Proxy @f) <> collectBindings' (Proxy @g)
  collectLayouts' _ = collectLayouts' (Proxy @f) <> collectLayouts' (Proxy @g)
  collectEntries' (f :*: g) = collectEntries' f <> collectEntries' g

instance (Bindable c) => Bindable' (K1 i c) where
  collectBindings' _ = collectBindings (Proxy @c)
  collectLayouts' _ = collectLayouts (Proxy @c)
  collectEntries' (K1 a) = collectEntries a

instance (Bindable' f) => Bindable' (M1 i t f) where
  collectBindings' _ = collectBindings' (Proxy @f)
  collectLayouts' _ = collectLayouts' (Proxy @f)
  collectEntries' (M1 a) = collectEntries' a

instance (KnownNat n) => Bindable (Binding n Sampler) where
  collectBindings _ = Bindings [BindingData {bType = T.pack "sampler", index = natVal (Proxy @n)}]
  collectLayouts _ =
    [ WGPUBindGroupLayoutEntry
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
    ]

  collectEntries (BindEntry a) = [a]
  collectEntries _ = undefined

instance (KnownNat n) => Bindable (Binding n Texture) where
  collectBindings _ = Bindings [BindingData {bType = T.pack "texture_2d<f32>", index = natVal (Proxy @n)}]
  collectLayouts _ =
    [ WGPUBindGroupLayoutEntry
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
    ]

  collectEntries (BindEntry a) = [a]
  collectEntries _ = undefined

instance Bindable ()

instance (KnownNat n, SingI (Primitive a)) => Bindable (Binding n (Expr (Primitive a))) where
  collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (Primitive a)), index = natVal (Proxy @n)}]
  collectLayouts _ = undefined

  collectEntries (BindEntry a) = [a]
  collectEntries _ = undefined

instance (KnownNat n, SingI (VectorType m a)) => Bindable (Binding n (Expr (VectorType m a))) where
  collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (VectorType m a)), index = natVal (Proxy @n)}]
  collectLayouts _ = undefined

  collectEntries (BindEntry a) = [a]
  collectEntries _ = undefined

instance (KnownNat n, SingI (ArrayType m a)) => Bindable (Binding n (Expr (ArrayType m a))) where
  collectBindings _ = Bindings [BindingData {bType = typeToWGSL (undefined :: Expr (ArrayType m a)), index = natVal (Proxy @n)}]
  collectLayouts _ = undefined

  collectEntries (BindEntry a) = [a]
  collectEntries _ = undefined

createBindLayout :: forall b. (Bindable b) => RenderDevice -> IO BindLayout
createBindLayout (RenderDevice device) = do
  let layouts = VS.fromList $ collectLayouts (Proxy @b)

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

createBindGroup :: forall b. (Bindable b) => RenderDevice -> BindLayout -> b -> IO (Ptr WGPUBindGroup)
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

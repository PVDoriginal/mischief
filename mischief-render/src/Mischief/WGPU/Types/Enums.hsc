#include "webgpu.h"

module Mischief.WGPU.Types.Enums where

import Foreign 
import Foreign.C.ConstPtr
import Foreign.C.Types
import Data.Void

import Data.Word (Word64)

newtype WGPUSType = WGPUSType CUInt deriving (Eq, Ord, Show)

instance Storable WGPUSType where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUSType x
  poke ptr (WGPUSType x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUSType_ShaderSourceSPIRV :: WGPUSType
wGPUSType_ShaderSourceSPIRV = WGPUSType #const WGPUSType_ShaderSourceSPIRV 

wGPUSType_ShaderSourceWGSL :: WGPUSType
wGPUSType_ShaderSourceWGSL = WGPUSType #const WGPUSType_ShaderSourceWGSL 

wGPUSType_RenderPassMaxDrawCount :: WGPUSType
wGPUSType_RenderPassMaxDrawCount = WGPUSType #const WGPUSType_RenderPassMaxDrawCount 

wGPUSType_SurfaceSourceMetalLayer :: WGPUSType
wGPUSType_SurfaceSourceMetalLayer = WGPUSType #const WGPUSType_SurfaceSourceMetalLayer 

wGPUSType_SurfaceSourceWindowsHWND :: WGPUSType
wGPUSType_SurfaceSourceWindowsHWND = WGPUSType #const WGPUSType_SurfaceSourceWindowsHWND 

wGPUSType_SurfaceSourceXlibWindow :: WGPUSType
wGPUSType_SurfaceSourceXlibWindow = WGPUSType #const WGPUSType_SurfaceSourceXlibWindow 

wGPUSType_SurfaceSourceWaylandSurface :: WGPUSType
wGPUSType_SurfaceSourceWaylandSurface = WGPUSType #const WGPUSType_SurfaceSourceWaylandSurface 

wGPUSType_SurfaceSourceAndroidNativeWindow :: WGPUSType
wGPUSType_SurfaceSourceAndroidNativeWindow = WGPUSType #const WGPUSType_SurfaceSourceAndroidNativeWindow 

wGPUSType_SurfaceSourceXCBWindow :: WGPUSType
wGPUSType_SurfaceSourceXCBWindow = WGPUSType #const WGPUSType_SurfaceSourceXCBWindow 

wGPUSType_SurfaceColorManagement :: WGPUSType
wGPUSType_SurfaceColorManagement = WGPUSType #const WGPUSType_SurfaceColorManagement 

wGPUSType_RequestAdapterWebXROptions :: WGPUSType
wGPUSType_RequestAdapterWebXROptions = WGPUSType #const WGPUSType_RequestAdapterWebXROptions 

wGPUSType_TextureComponentSwizzleDescriptor :: WGPUSType
wGPUSType_TextureComponentSwizzleDescriptor = WGPUSType #const WGPUSType_TextureComponentSwizzleDescriptor 

wGPUSType_ExternalTextureBindingLayout :: WGPUSType
wGPUSType_ExternalTextureBindingLayout = WGPUSType #const WGPUSType_ExternalTextureBindingLayout 

wGPUSType_ExternalTextureBindingEntry :: WGPUSType
wGPUSType_ExternalTextureBindingEntry = WGPUSType #const WGPUSType_ExternalTextureBindingEntry 

wGPUSType_CompatibilityModeLimits :: WGPUSType
wGPUSType_CompatibilityModeLimits = WGPUSType #const WGPUSType_CompatibilityModeLimits 

wGPUSType_TextureBindingViewDimension :: WGPUSType
wGPUSType_TextureBindingViewDimension = WGPUSType #const WGPUSType_TextureBindingViewDimension 

wGPUSType_Force32 :: WGPUSType
wGPUSType_Force32 = WGPUSType #const WGPUSType_Force32 



newtype WGPUFeatureLevel = WGPUFeatureLevel CUInt deriving (Eq, Ord, Show)


instance Storable WGPUFeatureLevel where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUFeatureLevel x
  poke ptr (WGPUFeatureLevel x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUFeatureLevel_Undefined :: WGPUFeatureLevel
wGPUFeatureLevel_Undefined = WGPUFeatureLevel #const WGPUFeatureLevel_Undefined

wGPUFeatureLevel_Compatibility :: WGPUFeatureLevel
wGPUFeatureLevel_Compatibility = WGPUFeatureLevel #const WGPUFeatureLevel_Compatibility

wGPUFeatureLevel_Core :: WGPUFeatureLevel
wGPUFeatureLevel_Core = WGPUFeatureLevel #const WGPUFeatureLevel_Core

wGPUFeatureLevel_Force32 :: WGPUFeatureLevel
wGPUFeatureLevel_Force32 = WGPUFeatureLevel #const WGPUFeatureLevel_Force32


newtype WGPUPowerPreference = WGPUPowerPreference CUInt deriving (Eq, Ord, Show)

instance Storable WGPUPowerPreference where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUPowerPreference x
  poke ptr (WGPUPowerPreference x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUPowerPreference_Undefined :: WGPUPowerPreference
wGPUPowerPreference_Undefined = WGPUPowerPreference #const WGPUPowerPreference_Undefined

wGPUPowerPreference_LowPower :: WGPUPowerPreference
wGPUPowerPreference_LowPower = WGPUPowerPreference #const WGPUPowerPreference_LowPower

wGPUPowerPreference_HighPerformance :: WGPUPowerPreference
wGPUPowerPreference_HighPerformance = WGPUPowerPreference #const WGPUPowerPreference_HighPerformance

wGPUPowerPreference_Force32 :: WGPUPowerPreference
wGPUPowerPreference_Force32 = WGPUPowerPreference #const WGPUPowerPreference_Force32

newtype WGPUBool = WGPUBool Bool deriving (Show, Eq)

instance Storable WGPUBool where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x :: CUInt <- peek (castPtr ptr)
    pure $ WGPUBool (x /= 0)
  poke ptr (WGPUBool x) = do
    let a :: Ptr CUInt = castPtr ptr
    if x then 
      poke a 1 
    else 
      poke a 0 

  
newtype WGPUBackendType = WGPUBackendType CUInt deriving (Eq, Ord, Show)


instance Storable WGPUBackendType where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUBackendType x
  poke ptr (WGPUBackendType x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x


wGPUBackendType_Undefined :: WGPUBackendType
wGPUBackendType_Undefined = WGPUBackendType #const WGPUBackendType_Undefined

wGPUBackendType_Null :: WGPUBackendType
wGPUBackendType_Null = WGPUBackendType #const WGPUBackendType_Null

wGPUBackendType_WebGPU :: WGPUBackendType
wGPUBackendType_WebGPU = WGPUBackendType #const WGPUBackendType_WebGPU

wGPUBackendType_D3D11 :: WGPUBackendType
wGPUBackendType_D3D11 = WGPUBackendType #const WGPUBackendType_D3D11

wGPUBackendType_D3D12 :: WGPUBackendType
wGPUBackendType_D3D12 = WGPUBackendType #const WGPUBackendType_D3D12

wGPUBackendType_Metal :: WGPUBackendType
wGPUBackendType_Metal = WGPUBackendType #const WGPUBackendType_Metal

wGPUBackendType_Vulkan :: WGPUBackendType
wGPUBackendType_Vulkan = WGPUBackendType #const WGPUBackendType_Vulkan

wGPUBackendType_OpenGL :: WGPUBackendType
wGPUBackendType_OpenGL = WGPUBackendType #const WGPUBackendType_OpenGL

wGPUBackendType_OpenGLES :: WGPUBackendType
wGPUBackendType_OpenGLES = WGPUBackendType #const WGPUBackendType_OpenGLES

wGPUBackendType_Force32 :: WGPUBackendType
wGPUBackendType_Force32 = WGPUBackendType #const WGPUBackendType_Force32


newtype WGPURequestAdapterStatus = WGPURequestAdapterStatus CUInt deriving (Eq, Ord, Show)

instance Storable WGPURequestAdapterStatus where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPURequestAdapterStatus x
  poke ptr (WGPURequestAdapterStatus x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x


wGPURequestAdapterStatus_Success :: WGPURequestAdapterStatus
wGPURequestAdapterStatus_Success = WGPURequestAdapterStatus #const WGPURequestAdapterStatus_Success

wGPURequestAdapterStatus_CallbackCancelled :: WGPURequestAdapterStatus
wGPURequestAdapterStatus_CallbackCancelled = WGPURequestAdapterStatus #const WGPURequestAdapterStatus_CallbackCancelled

wGPURequestAdapterStatus_Unavailable :: WGPURequestAdapterStatus
wGPURequestAdapterStatus_Unavailable = WGPURequestAdapterStatus #const WGPURequestAdapterStatus_Unavailable

wGPURequestAdapterStatus_Error :: WGPURequestAdapterStatus
wGPURequestAdapterStatus_Error = WGPURequestAdapterStatus #const WGPURequestAdapterStatus_Error

wGPURequestAdapterStatus_Force32 :: WGPURequestAdapterStatus
wGPURequestAdapterStatus_Force32 = WGPURequestAdapterStatus #const WGPURequestAdapterStatus_Force32


newtype WGPUCallbackMode = WGPUCallbackMode CUInt deriving (Eq, Ord, Show)

instance Storable WGPUCallbackMode where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUCallbackMode x
  poke ptr (WGPUCallbackMode x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x


wGPUCallbackMode_WaitAnyOnly :: WGPUCallbackMode
wGPUCallbackMode_WaitAnyOnly = WGPUCallbackMode #const WGPUCallbackMode_WaitAnyOnly

wGPUCallbackMode_AllowProcessEvents :: WGPUCallbackMode
wGPUCallbackMode_AllowProcessEvents = WGPUCallbackMode #const WGPUCallbackMode_AllowProcessEvents

wGPUCallbackMode_AllowSpontaneous :: WGPUCallbackMode
wGPUCallbackMode_AllowSpontaneous = WGPUCallbackMode #const WGPUCallbackMode_AllowSpontaneous

wGPUCallbackMode_Force32 :: WGPUCallbackMode
wGPUCallbackMode_Force32 = WGPUCallbackMode #const WGPUCallbackMode_Force32


newtype WGPURequestDeviceStatus = WGPURequestDeviceStatus CUInt deriving (Eq, Ord, Show)


instance Storable WGPURequestDeviceStatus where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPURequestDeviceStatus x
  poke ptr (WGPURequestDeviceStatus x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x


wGPURequestDeviceStatus_Success :: WGPURequestDeviceStatus
wGPURequestDeviceStatus_Success = WGPURequestDeviceStatus #const WGPURequestDeviceStatus_Success

wGPURequestDeviceStatus_CallbackCancelled :: WGPURequestDeviceStatus
wGPURequestDeviceStatus_CallbackCancelled = WGPURequestDeviceStatus #const WGPURequestDeviceStatus_CallbackCancelled

wGPURequestDeviceStatus_Error :: WGPURequestDeviceStatus
wGPURequestDeviceStatus_Error = WGPURequestDeviceStatus #const WGPURequestDeviceStatus_Error

wGPURequestDeviceStatus_Force32 :: WGPURequestDeviceStatus
wGPURequestDeviceStatus_Force32 = WGPURequestDeviceStatus #const WGPURequestDeviceStatus_Force32

newtype WGPUFlags = WGPUFlags Word64 deriving newtype (Show, Eq, Storable, Num)

newtype WGPUShaderStage = WGPUShaderStage WGPUFlags deriving newtype (Show, Eq, Storable, Num)

wGPUShaderStage_None :: WGPUShaderStage 
wGPUShaderStage_None =  WGPUShaderStage #const WGPUShaderStage_None

wGPUShaderStage_Vertex :: WGPUShaderStage 
wGPUShaderStage_Vertex =  WGPUShaderStage #const WGPUShaderStage_Vertex

wGPUShaderStage_Fragment :: WGPUShaderStage 
wGPUShaderStage_Fragment =  WGPUShaderStage #const WGPUShaderStage_Fragment

wGPUShaderStage_Compute :: WGPUShaderStage 
wGPUShaderStage_Compute =  WGPUShaderStage #const WGPUShaderStage_Compute


newtype WGPUBufferBindingType = WGPUBufferBindingType CUInt deriving (Eq, Ord, Show)

instance Storable WGPUBufferBindingType where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUBufferBindingType x
  poke ptr (WGPUBufferBindingType x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUBufferBindingType_BindingNotUsed :: WGPUBufferBindingType
wGPUBufferBindingType_BindingNotUsed = WGPUBufferBindingType #const WGPUBufferBindingType_BindingNotUsed

wGPUBufferBindingType_Undefined :: WGPUBufferBindingType
wGPUBufferBindingType_Undefined = WGPUBufferBindingType #const WGPUBufferBindingType_Undefined

wGPUBufferBindingType_Uniform :: WGPUBufferBindingType
wGPUBufferBindingType_Uniform = WGPUBufferBindingType #const WGPUBufferBindingType_Uniform

wGPUBufferBindingType_Storage :: WGPUBufferBindingType
wGPUBufferBindingType_Storage = WGPUBufferBindingType #const WGPUBufferBindingType_Storage

wGPUBufferBindingType_ReadOnlyStorage :: WGPUBufferBindingType
wGPUBufferBindingType_ReadOnlyStorage = WGPUBufferBindingType #const WGPUBufferBindingType_ReadOnlyStorage

wGPUBufferBindingType_Force32 :: WGPUBufferBindingType
wGPUBufferBindingType_Force32 = WGPUBufferBindingType #const WGPUBufferBindingType_Force32

newtype WGPUBlendFactor = WGPUBlendFactor CUInt deriving (Eq, Ord, Show)

instance Storable WGPUBlendFactor where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUBlendFactor x
  poke ptr (WGPUBlendFactor x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUBlendFactor_Undefined :: WGPUBlendFactor
wGPUBlendFactor_Undefined = WGPUBlendFactor #const WGPUBlendFactor_Undefined

wGPUBlendFactor_Zero :: WGPUBlendFactor
wGPUBlendFactor_Zero = WGPUBlendFactor #const WGPUBlendFactor_Zero

wGPUBlendFactor_One :: WGPUBlendFactor
wGPUBlendFactor_One = WGPUBlendFactor #const WGPUBlendFactor_One

wGPUBlendFactor_Src :: WGPUBlendFactor
wGPUBlendFactor_Src = WGPUBlendFactor #const WGPUBlendFactor_Src

wGPUBlendFactor_OneMinusSrc :: WGPUBlendFactor
wGPUBlendFactor_OneMinusSrc = WGPUBlendFactor #const WGPUBlendFactor_OneMinusSrc

wGPUBlendFactor_SrcAlpha :: WGPUBlendFactor
wGPUBlendFactor_SrcAlpha = WGPUBlendFactor #const WGPUBlendFactor_SrcAlpha

wGPUBlendFactor_OneMinusSrcAlpha :: WGPUBlendFactor
wGPUBlendFactor_OneMinusSrcAlpha = WGPUBlendFactor #const WGPUBlendFactor_OneMinusSrcAlpha

wGPUBlendFactor_Dst :: WGPUBlendFactor
wGPUBlendFactor_Dst = WGPUBlendFactor #const WGPUBlendFactor_Dst

wGPUBlendFactor_OneMinusDst :: WGPUBlendFactor
wGPUBlendFactor_OneMinusDst = WGPUBlendFactor #const WGPUBlendFactor_OneMinusDst

wGPUBlendFactor_DstAlpha :: WGPUBlendFactor
wGPUBlendFactor_DstAlpha = WGPUBlendFactor #const WGPUBlendFactor_DstAlpha

wGPUBlendFactor_OneMinusDstAlpha :: WGPUBlendFactor
wGPUBlendFactor_OneMinusDstAlpha = WGPUBlendFactor #const WGPUBlendFactor_OneMinusDstAlpha

wGPUBlendFactor_SrcAlphaSaturated :: WGPUBlendFactor
wGPUBlendFactor_SrcAlphaSaturated = WGPUBlendFactor #const WGPUBlendFactor_SrcAlphaSaturated

wGPUBlendFactor_Constant :: WGPUBlendFactor
wGPUBlendFactor_Constant = WGPUBlendFactor #const WGPUBlendFactor_Constant

wGPUBlendFactor_OneMinusConstant :: WGPUBlendFactor
wGPUBlendFactor_OneMinusConstant = WGPUBlendFactor #const WGPUBlendFactor_OneMinusConstant

wGPUBlendFactor_Src1 :: WGPUBlendFactor
wGPUBlendFactor_Src1 = WGPUBlendFactor #const WGPUBlendFactor_Src1

wGPUBlendFactor_OneMinusSrc1 :: WGPUBlendFactor
wGPUBlendFactor_OneMinusSrc1 = WGPUBlendFactor #const WGPUBlendFactor_OneMinusSrc1

wGPUBlendFactor_Src1Alpha :: WGPUBlendFactor
wGPUBlendFactor_Src1Alpha = WGPUBlendFactor #const WGPUBlendFactor_Src1Alpha

wGPUBlendFactor_OneMinusSrc1Alpha :: WGPUBlendFactor
wGPUBlendFactor_OneMinusSrc1Alpha = WGPUBlendFactor #const WGPUBlendFactor_OneMinusSrc1Alpha

wGPUBlendFactor_Force32 :: WGPUBlendFactor
wGPUBlendFactor_Force32 = WGPUBlendFactor #const WGPUBlendFactor_Force32


newtype WGPUBlendOperation = WGPUBlendOperation CUInt deriving (Eq, Ord, Show)

instance Storable WGPUBlendOperation where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUBlendOperation x
  poke ptr (WGPUBlendOperation x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUBlendOperation_Undefined :: WGPUBlendOperation
wGPUBlendOperation_Undefined = WGPUBlendOperation #const WGPUBlendOperation_Undefined

wGPUBlendOperation_Add :: WGPUBlendOperation
wGPUBlendOperation_Add = WGPUBlendOperation #const WGPUBlendOperation_Add

wGPUBlendOperation_Subtract :: WGPUBlendOperation
wGPUBlendOperation_Subtract = WGPUBlendOperation #const WGPUBlendOperation_Subtract

wGPUBlendOperation_ReverseSubtract :: WGPUBlendOperation
wGPUBlendOperation_ReverseSubtract = WGPUBlendOperation #const WGPUBlendOperation_ReverseSubtract

wGPUBlendOperation_Min :: WGPUBlendOperation
wGPUBlendOperation_Min = WGPUBlendOperation #const WGPUBlendOperation_Min

wGPUBlendOperation_Max :: WGPUBlendOperation
wGPUBlendOperation_Max = WGPUBlendOperation #const WGPUBlendOperation_Max

wGPUBlendOperation_Force32 :: WGPUBlendOperation
wGPUBlendOperation_Force32 = WGPUBlendOperation #const WGPUBlendOperation_Force32

newtype WGPUBufferMapState = WGPUBufferMapState CUInt deriving (Eq, Ord, Show)

instance Storable WGPUBufferMapState where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUBufferMapState x
  poke ptr (WGPUBufferMapState x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUBufferMapState_Unmapped :: WGPUBufferMapState
wGPUBufferMapState_Unmapped = WGPUBufferMapState #const WGPUBufferMapState_Unmapped

wGPUBufferMapState_Pending :: WGPUBufferMapState
wGPUBufferMapState_Pending = WGPUBufferMapState #const WGPUBufferMapState_Pending

wGPUBufferMapState_Mapped :: WGPUBufferMapState
wGPUBufferMapState_Mapped = WGPUBufferMapState #const WGPUBufferMapState_Mapped

wGPUBufferMapState_Force32 :: WGPUBufferMapState
wGPUBufferMapState_Force32 = WGPUBufferMapState #const WGPUBufferMapState_Force32


newtype WGPUTextureSampleType = WGPUTextureSampleType CUInt deriving (Eq, Ord, Show)

instance Storable WGPUTextureSampleType where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUTextureSampleType x
  poke ptr (WGPUTextureSampleType x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUTextureSampleType_BindingNotUsed :: WGPUTextureSampleType
wGPUTextureSampleType_BindingNotUsed = WGPUTextureSampleType #const WGPUTextureSampleType_BindingNotUsed

wGPUTextureSampleType_Undefined :: WGPUTextureSampleType
wGPUTextureSampleType_Undefined = WGPUTextureSampleType #const WGPUTextureSampleType_Undefined

wGPUTextureSampleType_Float :: WGPUTextureSampleType
wGPUTextureSampleType_Float = WGPUTextureSampleType #const WGPUTextureSampleType_Float

wGPUTextureSampleType_UnfilterableFloat :: WGPUTextureSampleType
wGPUTextureSampleType_UnfilterableFloat = WGPUTextureSampleType #const WGPUTextureSampleType_UnfilterableFloat

wGPUTextureSampleType_Depth :: WGPUTextureSampleType
wGPUTextureSampleType_Depth = WGPUTextureSampleType #const WGPUTextureSampleType_Depth

wGPUTextureSampleType_Sint :: WGPUTextureSampleType
wGPUTextureSampleType_Sint = WGPUTextureSampleType #const WGPUTextureSampleType_Sint

wGPUTextureSampleType_Uint :: WGPUTextureSampleType
wGPUTextureSampleType_Uint = WGPUTextureSampleType #const WGPUTextureSampleType_Uint

wGPUTextureSampleType_Force32 :: WGPUTextureSampleType
wGPUTextureSampleType_Force32 = WGPUTextureSampleType #const WGPUTextureSampleType_Force32

newtype WGPUTextureViewDimension = WGPUTextureViewDimension CUInt deriving (Eq, Ord, Show)

instance Storable WGPUTextureViewDimension where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUTextureViewDimension x
  poke ptr (WGPUTextureViewDimension x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUTextureViewDimension_Undefined :: WGPUTextureViewDimension
wGPUTextureViewDimension_Undefined = WGPUTextureViewDimension #const WGPUTextureViewDimension_Undefined

wGPUTextureViewDimension_1D :: WGPUTextureViewDimension
wGPUTextureViewDimension_1D = WGPUTextureViewDimension #const WGPUTextureViewDimension_1D

wGPUTextureViewDimension_2D :: WGPUTextureViewDimension
wGPUTextureViewDimension_2D = WGPUTextureViewDimension #const WGPUTextureViewDimension_2D

wGPUTextureViewDimension_2DArray :: WGPUTextureViewDimension
wGPUTextureViewDimension_2DArray = WGPUTextureViewDimension #const WGPUTextureViewDimension_2DArray

wGPUTextureViewDimension_Cube :: WGPUTextureViewDimension
wGPUTextureViewDimension_Cube = WGPUTextureViewDimension #const WGPUTextureViewDimension_Cube

wGPUTextureViewDimension_CubeArray :: WGPUTextureViewDimension
wGPUTextureViewDimension_CubeArray = WGPUTextureViewDimension #const WGPUTextureViewDimension_CubeArray

wGPUTextureViewDimension_3D :: WGPUTextureViewDimension
wGPUTextureViewDimension_3D = WGPUTextureViewDimension #const WGPUTextureViewDimension_3D

wGPUTextureViewDimension_Force32 :: WGPUTextureViewDimension
wGPUTextureViewDimension_Force32 = WGPUTextureViewDimension #const WGPUTextureViewDimension_Force32


newtype WGPUToneMappingMode = WGPUToneMappingMode CUInt deriving (Eq, Ord, Show)

instance Storable WGPUToneMappingMode where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUToneMappingMode x
  poke ptr (WGPUToneMappingMode x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUToneMappingMode_Standard :: WGPUToneMappingMode
wGPUToneMappingMode_Standard = WGPUToneMappingMode #const WGPUToneMappingMode_Standard

wGPUToneMappingMode_Extended :: WGPUToneMappingMode
wGPUToneMappingMode_Extended = WGPUToneMappingMode #const WGPUToneMappingMode_Extended

wGPUToneMappingMode_Force32 :: WGPUToneMappingMode
wGPUToneMappingMode_Force32 = WGPUToneMappingMode #const WGPUToneMappingMode_Force32


newtype WGPUVertexFormat = WGPUVertexFormat CUInt deriving (Eq, Ord, Show)

instance Storable WGPUVertexFormat where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUVertexFormat x
  poke ptr (WGPUVertexFormat x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUVertexFormat_Uint8 :: WGPUVertexFormat
wGPUVertexFormat_Uint8 = WGPUVertexFormat #const WGPUVertexFormat_Uint8

wGPUVertexFormat_Uint8x2 :: WGPUVertexFormat
wGPUVertexFormat_Uint8x2 = WGPUVertexFormat #const WGPUVertexFormat_Uint8x2

wGPUVertexFormat_Uint8x4 :: WGPUVertexFormat
wGPUVertexFormat_Uint8x4 = WGPUVertexFormat #const WGPUVertexFormat_Uint8x4

wGPUVertexFormat_Sint8 :: WGPUVertexFormat
wGPUVertexFormat_Sint8 = WGPUVertexFormat #const WGPUVertexFormat_Sint8

wGPUVertexFormat_Sint8x2 :: WGPUVertexFormat
wGPUVertexFormat_Sint8x2 = WGPUVertexFormat #const WGPUVertexFormat_Sint8x2

wGPUVertexFormat_Sint8x4 :: WGPUVertexFormat
wGPUVertexFormat_Sint8x4 = WGPUVertexFormat #const WGPUVertexFormat_Sint8x4

wGPUVertexFormat_Unorm8 :: WGPUVertexFormat
wGPUVertexFormat_Unorm8 = WGPUVertexFormat #const WGPUVertexFormat_Unorm8

wGPUVertexFormat_Unorm8x2 :: WGPUVertexFormat
wGPUVertexFormat_Unorm8x2 = WGPUVertexFormat #const WGPUVertexFormat_Unorm8x2

wGPUVertexFormat_Unorm8x4 :: WGPUVertexFormat
wGPUVertexFormat_Unorm8x4 = WGPUVertexFormat #const WGPUVertexFormat_Unorm8x4

wGPUVertexFormat_Snorm8 :: WGPUVertexFormat
wGPUVertexFormat_Snorm8 = WGPUVertexFormat #const WGPUVertexFormat_Snorm8

wGPUVertexFormat_Snorm8x2 :: WGPUVertexFormat
wGPUVertexFormat_Snorm8x2 = WGPUVertexFormat #const WGPUVertexFormat_Snorm8x2

wGPUVertexFormat_Snorm8x4 :: WGPUVertexFormat
wGPUVertexFormat_Snorm8x4 = WGPUVertexFormat #const WGPUVertexFormat_Snorm8x4

wGPUVertexFormat_Uint16 :: WGPUVertexFormat
wGPUVertexFormat_Uint16 = WGPUVertexFormat #const WGPUVertexFormat_Uint16

wGPUVertexFormat_Uint16x2 :: WGPUVertexFormat
wGPUVertexFormat_Uint16x2 = WGPUVertexFormat #const WGPUVertexFormat_Uint16x2

wGPUVertexFormat_Uint16x4 :: WGPUVertexFormat
wGPUVertexFormat_Uint16x4 = WGPUVertexFormat #const WGPUVertexFormat_Uint16x4

wGPUVertexFormat_Sint16 :: WGPUVertexFormat
wGPUVertexFormat_Sint16 = WGPUVertexFormat #const WGPUVertexFormat_Sint16

wGPUVertexFormat_Sint16x2 :: WGPUVertexFormat
wGPUVertexFormat_Sint16x2 = WGPUVertexFormat #const WGPUVertexFormat_Sint16x2

wGPUVertexFormat_Sint16x4 :: WGPUVertexFormat
wGPUVertexFormat_Sint16x4 = WGPUVertexFormat #const WGPUVertexFormat_Sint16x4

wGPUVertexFormat_Unorm16 :: WGPUVertexFormat
wGPUVertexFormat_Unorm16 = WGPUVertexFormat #const WGPUVertexFormat_Unorm16

wGPUVertexFormat_Unorm16x2 :: WGPUVertexFormat
wGPUVertexFormat_Unorm16x2 = WGPUVertexFormat #const WGPUVertexFormat_Unorm16x2

wGPUVertexFormat_Unorm16x4 :: WGPUVertexFormat
wGPUVertexFormat_Unorm16x4 = WGPUVertexFormat #const WGPUVertexFormat_Unorm16x4

wGPUVertexFormat_Snorm16 :: WGPUVertexFormat
wGPUVertexFormat_Snorm16 = WGPUVertexFormat #const WGPUVertexFormat_Snorm16

wGPUVertexFormat_Snorm16x2 :: WGPUVertexFormat
wGPUVertexFormat_Snorm16x2 = WGPUVertexFormat #const WGPUVertexFormat_Snorm16x2

wGPUVertexFormat_Snorm16x4 :: WGPUVertexFormat
wGPUVertexFormat_Snorm16x4 = WGPUVertexFormat #const WGPUVertexFormat_Snorm16x4

wGPUVertexFormat_Float16 :: WGPUVertexFormat
wGPUVertexFormat_Float16 = WGPUVertexFormat #const WGPUVertexFormat_Float16

wGPUVertexFormat_Float16x2 :: WGPUVertexFormat
wGPUVertexFormat_Float16x2 = WGPUVertexFormat #const WGPUVertexFormat_Float16x2

wGPUVertexFormat_Float16x4 :: WGPUVertexFormat
wGPUVertexFormat_Float16x4 = WGPUVertexFormat #const WGPUVertexFormat_Float16x4

wGPUVertexFormat_Float32 :: WGPUVertexFormat
wGPUVertexFormat_Float32 = WGPUVertexFormat #const WGPUVertexFormat_Float32

wGPUVertexFormat_Float32x2 :: WGPUVertexFormat
wGPUVertexFormat_Float32x2 = WGPUVertexFormat #const WGPUVertexFormat_Float32x2

wGPUVertexFormat_Float32x3 :: WGPUVertexFormat
wGPUVertexFormat_Float32x3 = WGPUVertexFormat #const WGPUVertexFormat_Float32x3

wGPUVertexFormat_Float32x4 :: WGPUVertexFormat
wGPUVertexFormat_Float32x4 = WGPUVertexFormat #const WGPUVertexFormat_Float32x4

wGPUVertexFormat_Uint32 :: WGPUVertexFormat
wGPUVertexFormat_Uint32 = WGPUVertexFormat #const WGPUVertexFormat_Uint32

wGPUVertexFormat_Uint32x2 :: WGPUVertexFormat
wGPUVertexFormat_Uint32x2 = WGPUVertexFormat #const WGPUVertexFormat_Uint32x2

wGPUVertexFormat_Uint32x3 :: WGPUVertexFormat
wGPUVertexFormat_Uint32x3 = WGPUVertexFormat #const WGPUVertexFormat_Uint32x3

wGPUVertexFormat_Uint32x4 :: WGPUVertexFormat
wGPUVertexFormat_Uint32x4 = WGPUVertexFormat #const WGPUVertexFormat_Uint32x4

wGPUVertexFormat_Sint32 :: WGPUVertexFormat
wGPUVertexFormat_Sint32 = WGPUVertexFormat #const WGPUVertexFormat_Sint32

wGPUVertexFormat_Sint32x2 :: WGPUVertexFormat
wGPUVertexFormat_Sint32x2 = WGPUVertexFormat #const WGPUVertexFormat_Sint32x2

wGPUVertexFormat_Sint32x3 :: WGPUVertexFormat
wGPUVertexFormat_Sint32x3 = WGPUVertexFormat #const WGPUVertexFormat_Sint32x3

wGPUVertexFormat_Sint32x4 :: WGPUVertexFormat
wGPUVertexFormat_Sint32x4 = WGPUVertexFormat #const WGPUVertexFormat_Sint32x4

wGPUVertexFormat_Unorm10_10_10_2 :: WGPUVertexFormat
wGPUVertexFormat_Unorm10_10_10_2 = WGPUVertexFormat #const WGPUVertexFormat_Unorm10_10_10_2

wGPUVertexFormat_Unorm8x4BGRA :: WGPUVertexFormat
wGPUVertexFormat_Unorm8x4BGRA = WGPUVertexFormat #const WGPUVertexFormat_Unorm8x4BGRA

wGPUVertexFormat_Force32 :: WGPUVertexFormat
wGPUVertexFormat_Force32 = WGPUVertexFormat #const WGPUVertexFormat_Force32

newtype WGPUVertexStepMode = WGPUVertexStepMode CUInt deriving (Eq, Ord, Show)

instance Storable WGPUVertexStepMode where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUVertexStepMode x
  poke ptr (WGPUVertexStepMode x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUVertexStepMode_Undefined :: WGPUVertexStepMode
wGPUVertexStepMode_Undefined = WGPUVertexStepMode #const WGPUVertexStepMode_Undefined

wGPUVertexStepMode_Vertex :: WGPUVertexStepMode
wGPUVertexStepMode_Vertex = WGPUVertexStepMode #const WGPUVertexStepMode_Vertex

wGPUVertexStepMode_Instance :: WGPUVertexStepMode
wGPUVertexStepMode_Instance = WGPUVertexStepMode #const WGPUVertexStepMode_Instance

wGPUVertexStepMode_Force32 :: WGPUVertexStepMode
wGPUVertexStepMode_Force32 = WGPUVertexStepMode #const WGPUVertexStepMode_Force32

newtype WGPUStorageTextureAccess = WGPUStorageTextureAccess CUInt deriving (Eq, Ord, Show)

instance Storable WGPUStorageTextureAccess where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUStorageTextureAccess x
  poke ptr (WGPUStorageTextureAccess x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUStorageTextureAccess_BindingNotUsed :: WGPUStorageTextureAccess
wGPUStorageTextureAccess_BindingNotUsed = WGPUStorageTextureAccess #const WGPUStorageTextureAccess_BindingNotUsed

wGPUStorageTextureAccess_Undefined :: WGPUStorageTextureAccess
wGPUStorageTextureAccess_Undefined = WGPUStorageTextureAccess #const WGPUStorageTextureAccess_Undefined

wGPUStorageTextureAccess_WriteOnly :: WGPUStorageTextureAccess
wGPUStorageTextureAccess_WriteOnly = WGPUStorageTextureAccess #const WGPUStorageTextureAccess_WriteOnly

wGPUStorageTextureAccess_ReadOnly :: WGPUStorageTextureAccess
wGPUStorageTextureAccess_ReadOnly = WGPUStorageTextureAccess #const WGPUStorageTextureAccess_ReadOnly

wGPUStorageTextureAccess_ReadWrite :: WGPUStorageTextureAccess
wGPUStorageTextureAccess_ReadWrite = WGPUStorageTextureAccess #const WGPUStorageTextureAccess_ReadWrite

wGPUStorageTextureAccess_Force32 :: WGPUStorageTextureAccess
wGPUStorageTextureAccess_Force32 = WGPUStorageTextureAccess #const WGPUStorageTextureAccess_Force32


newtype WGPUTextureFormat = WGPUTextureFormat CUInt deriving (Eq, Ord, Show)

instance Storable WGPUTextureFormat where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUTextureFormat x
  poke ptr (WGPUTextureFormat x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUTextureFormat_Undefined :: WGPUTextureFormat
wGPUTextureFormat_Undefined = WGPUTextureFormat #const WGPUTextureFormat_Undefined

wGPUTextureFormat_R8Unorm :: WGPUTextureFormat
wGPUTextureFormat_R8Unorm = WGPUTextureFormat #const WGPUTextureFormat_R8Unorm

wGPUTextureFormat_R8Snorm :: WGPUTextureFormat
wGPUTextureFormat_R8Snorm = WGPUTextureFormat #const WGPUTextureFormat_R8Snorm

wGPUTextureFormat_R8Uint :: WGPUTextureFormat
wGPUTextureFormat_R8Uint = WGPUTextureFormat #const WGPUTextureFormat_R8Uint

wGPUTextureFormat_R8Sint :: WGPUTextureFormat
wGPUTextureFormat_R8Sint = WGPUTextureFormat #const WGPUTextureFormat_R8Sint

wGPUTextureFormat_R16Unorm :: WGPUTextureFormat
wGPUTextureFormat_R16Unorm = WGPUTextureFormat #const WGPUTextureFormat_R16Unorm

wGPUTextureFormat_R16Snorm :: WGPUTextureFormat
wGPUTextureFormat_R16Snorm = WGPUTextureFormat #const WGPUTextureFormat_R16Snorm

wGPUTextureFormat_R16Uint :: WGPUTextureFormat
wGPUTextureFormat_R16Uint = WGPUTextureFormat #const WGPUTextureFormat_R16Uint

wGPUTextureFormat_R16Sint :: WGPUTextureFormat
wGPUTextureFormat_R16Sint = WGPUTextureFormat #const WGPUTextureFormat_R16Sint

wGPUTextureFormat_R16Float :: WGPUTextureFormat
wGPUTextureFormat_R16Float = WGPUTextureFormat #const WGPUTextureFormat_R16Float

wGPUTextureFormat_RG8Unorm :: WGPUTextureFormat
wGPUTextureFormat_RG8Unorm = WGPUTextureFormat #const WGPUTextureFormat_RG8Unorm

wGPUTextureFormat_RG8Snorm :: WGPUTextureFormat
wGPUTextureFormat_RG8Snorm = WGPUTextureFormat #const WGPUTextureFormat_RG8Snorm

wGPUTextureFormat_RG8Uint :: WGPUTextureFormat
wGPUTextureFormat_RG8Uint = WGPUTextureFormat #const WGPUTextureFormat_RG8Uint

wGPUTextureFormat_RG8Sint :: WGPUTextureFormat
wGPUTextureFormat_RG8Sint = WGPUTextureFormat #const WGPUTextureFormat_RG8Sint

wGPUTextureFormat_R32Float :: WGPUTextureFormat
wGPUTextureFormat_R32Float = WGPUTextureFormat #const WGPUTextureFormat_R32Float

wGPUTextureFormat_R32Uint :: WGPUTextureFormat
wGPUTextureFormat_R32Uint = WGPUTextureFormat #const WGPUTextureFormat_R32Uint

wGPUTextureFormat_R32Sint :: WGPUTextureFormat
wGPUTextureFormat_R32Sint = WGPUTextureFormat #const WGPUTextureFormat_R32Sint

wGPUTextureFormat_RG16Unorm :: WGPUTextureFormat
wGPUTextureFormat_RG16Unorm = WGPUTextureFormat #const WGPUTextureFormat_RG16Unorm

wGPUTextureFormat_RG16Snorm :: WGPUTextureFormat
wGPUTextureFormat_RG16Snorm = WGPUTextureFormat #const WGPUTextureFormat_RG16Snorm

wGPUTextureFormat_RG16Uint :: WGPUTextureFormat
wGPUTextureFormat_RG16Uint = WGPUTextureFormat #const WGPUTextureFormat_RG16Uint

wGPUTextureFormat_RG16Sint :: WGPUTextureFormat
wGPUTextureFormat_RG16Sint = WGPUTextureFormat #const WGPUTextureFormat_RG16Sint

wGPUTextureFormat_RG16Float :: WGPUTextureFormat
wGPUTextureFormat_RG16Float = WGPUTextureFormat #const WGPUTextureFormat_RG16Float

wGPUTextureFormat_RGBA8Unorm :: WGPUTextureFormat
wGPUTextureFormat_RGBA8Unorm = WGPUTextureFormat #const WGPUTextureFormat_RGBA8Unorm

wGPUTextureFormat_RGBA8UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_RGBA8UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_RGBA8UnormSrgb

wGPUTextureFormat_RGBA8Snorm :: WGPUTextureFormat
wGPUTextureFormat_RGBA8Snorm = WGPUTextureFormat #const WGPUTextureFormat_RGBA8Snorm

wGPUTextureFormat_RGBA8Uint :: WGPUTextureFormat
wGPUTextureFormat_RGBA8Uint = WGPUTextureFormat #const WGPUTextureFormat_RGBA8Uint

wGPUTextureFormat_RGBA8Sint :: WGPUTextureFormat
wGPUTextureFormat_RGBA8Sint = WGPUTextureFormat #const WGPUTextureFormat_RGBA8Sint

wGPUTextureFormat_BGRA8Unorm :: WGPUTextureFormat
wGPUTextureFormat_BGRA8Unorm = WGPUTextureFormat #const WGPUTextureFormat_BGRA8Unorm

wGPUTextureFormat_BGRA8UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_BGRA8UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_BGRA8UnormSrgb

wGPUTextureFormat_RGB10A2Uint :: WGPUTextureFormat
wGPUTextureFormat_RGB10A2Uint = WGPUTextureFormat #const WGPUTextureFormat_RGB10A2Uint

wGPUTextureFormat_RGB10A2Unorm :: WGPUTextureFormat
wGPUTextureFormat_RGB10A2Unorm = WGPUTextureFormat #const WGPUTextureFormat_RGB10A2Unorm

wGPUTextureFormat_RG11B10Ufloat :: WGPUTextureFormat
wGPUTextureFormat_RG11B10Ufloat = WGPUTextureFormat #const WGPUTextureFormat_RG11B10Ufloat

wGPUTextureFormat_RGB9E5Ufloat :: WGPUTextureFormat
wGPUTextureFormat_RGB9E5Ufloat = WGPUTextureFormat #const WGPUTextureFormat_RGB9E5Ufloat

wGPUTextureFormat_RG32Float :: WGPUTextureFormat
wGPUTextureFormat_RG32Float = WGPUTextureFormat #const WGPUTextureFormat_RG32Float

wGPUTextureFormat_RG32Uint :: WGPUTextureFormat
wGPUTextureFormat_RG32Uint = WGPUTextureFormat #const WGPUTextureFormat_RG32Uint

wGPUTextureFormat_RG32Sint :: WGPUTextureFormat
wGPUTextureFormat_RG32Sint = WGPUTextureFormat #const WGPUTextureFormat_RG32Sint

wGPUTextureFormat_RGBA16Unorm :: WGPUTextureFormat
wGPUTextureFormat_RGBA16Unorm = WGPUTextureFormat #const WGPUTextureFormat_RGBA16Unorm

wGPUTextureFormat_RGBA16Snorm :: WGPUTextureFormat
wGPUTextureFormat_RGBA16Snorm = WGPUTextureFormat #const WGPUTextureFormat_RGBA16Snorm

wGPUTextureFormat_RGBA16Uint :: WGPUTextureFormat
wGPUTextureFormat_RGBA16Uint = WGPUTextureFormat #const WGPUTextureFormat_RGBA16Uint

wGPUTextureFormat_RGBA16Sint :: WGPUTextureFormat
wGPUTextureFormat_RGBA16Sint = WGPUTextureFormat #const WGPUTextureFormat_RGBA16Sint

wGPUTextureFormat_RGBA16Float :: WGPUTextureFormat
wGPUTextureFormat_RGBA16Float = WGPUTextureFormat #const WGPUTextureFormat_RGBA16Float

wGPUTextureFormat_RGBA32Float :: WGPUTextureFormat
wGPUTextureFormat_RGBA32Float = WGPUTextureFormat #const WGPUTextureFormat_RGBA32Float

wGPUTextureFormat_RGBA32Uint :: WGPUTextureFormat
wGPUTextureFormat_RGBA32Uint = WGPUTextureFormat #const WGPUTextureFormat_RGBA32Uint

wGPUTextureFormat_RGBA32Sint :: WGPUTextureFormat
wGPUTextureFormat_RGBA32Sint = WGPUTextureFormat #const WGPUTextureFormat_RGBA32Sint

wGPUTextureFormat_Stencil8 :: WGPUTextureFormat
wGPUTextureFormat_Stencil8 = WGPUTextureFormat #const WGPUTextureFormat_Stencil8

wGPUTextureFormat_Depth16Unorm :: WGPUTextureFormat
wGPUTextureFormat_Depth16Unorm = WGPUTextureFormat #const WGPUTextureFormat_Depth16Unorm

wGPUTextureFormat_Depth24Plus :: WGPUTextureFormat
wGPUTextureFormat_Depth24Plus = WGPUTextureFormat #const WGPUTextureFormat_Depth24Plus

wGPUTextureFormat_Depth24PlusStencil8 :: WGPUTextureFormat
wGPUTextureFormat_Depth24PlusStencil8 = WGPUTextureFormat #const WGPUTextureFormat_Depth24PlusStencil8

wGPUTextureFormat_Depth32Float :: WGPUTextureFormat
wGPUTextureFormat_Depth32Float = WGPUTextureFormat #const WGPUTextureFormat_Depth32Float

wGPUTextureFormat_Depth32FloatStencil8 :: WGPUTextureFormat
wGPUTextureFormat_Depth32FloatStencil8 = WGPUTextureFormat #const WGPUTextureFormat_Depth32FloatStencil8

wGPUTextureFormat_BC1RGBAUnorm :: WGPUTextureFormat
wGPUTextureFormat_BC1RGBAUnorm = WGPUTextureFormat #const WGPUTextureFormat_BC1RGBAUnorm

wGPUTextureFormat_BC1RGBAUnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_BC1RGBAUnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_BC1RGBAUnormSrgb

wGPUTextureFormat_BC2RGBAUnorm :: WGPUTextureFormat
wGPUTextureFormat_BC2RGBAUnorm = WGPUTextureFormat #const WGPUTextureFormat_BC2RGBAUnorm

wGPUTextureFormat_BC2RGBAUnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_BC2RGBAUnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_BC2RGBAUnormSrgb

wGPUTextureFormat_BC3RGBAUnorm :: WGPUTextureFormat
wGPUTextureFormat_BC3RGBAUnorm = WGPUTextureFormat #const WGPUTextureFormat_BC3RGBAUnorm

wGPUTextureFormat_BC3RGBAUnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_BC3RGBAUnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_BC3RGBAUnormSrgb

wGPUTextureFormat_BC4RUnorm :: WGPUTextureFormat
wGPUTextureFormat_BC4RUnorm = WGPUTextureFormat #const WGPUTextureFormat_BC4RUnorm

wGPUTextureFormat_BC4RSnorm :: WGPUTextureFormat
wGPUTextureFormat_BC4RSnorm = WGPUTextureFormat #const WGPUTextureFormat_BC4RSnorm

wGPUTextureFormat_BC5RGUnorm :: WGPUTextureFormat
wGPUTextureFormat_BC5RGUnorm = WGPUTextureFormat #const WGPUTextureFormat_BC5RGUnorm

wGPUTextureFormat_BC5RGSnorm :: WGPUTextureFormat
wGPUTextureFormat_BC5RGSnorm = WGPUTextureFormat #const WGPUTextureFormat_BC5RGSnorm

wGPUTextureFormat_BC6HRGBUfloat :: WGPUTextureFormat
wGPUTextureFormat_BC6HRGBUfloat = WGPUTextureFormat #const WGPUTextureFormat_BC6HRGBUfloat

wGPUTextureFormat_BC6HRGBFloat :: WGPUTextureFormat
wGPUTextureFormat_BC6HRGBFloat = WGPUTextureFormat #const WGPUTextureFormat_BC6HRGBFloat

wGPUTextureFormat_BC7RGBAUnorm :: WGPUTextureFormat
wGPUTextureFormat_BC7RGBAUnorm = WGPUTextureFormat #const WGPUTextureFormat_BC7RGBAUnorm

wGPUTextureFormat_BC7RGBAUnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_BC7RGBAUnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_BC7RGBAUnormSrgb

wGPUTextureFormat_ETC2RGB8Unorm :: WGPUTextureFormat
wGPUTextureFormat_ETC2RGB8Unorm = WGPUTextureFormat #const WGPUTextureFormat_ETC2RGB8Unorm

wGPUTextureFormat_ETC2RGB8UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ETC2RGB8UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ETC2RGB8UnormSrgb

wGPUTextureFormat_ETC2RGB8A1Unorm :: WGPUTextureFormat
wGPUTextureFormat_ETC2RGB8A1Unorm = WGPUTextureFormat #const WGPUTextureFormat_ETC2RGB8A1Unorm

wGPUTextureFormat_ETC2RGB8A1UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ETC2RGB8A1UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ETC2RGB8A1UnormSrgb

wGPUTextureFormat_ETC2RGBA8Unorm :: WGPUTextureFormat
wGPUTextureFormat_ETC2RGBA8Unorm = WGPUTextureFormat #const WGPUTextureFormat_ETC2RGBA8Unorm

wGPUTextureFormat_ETC2RGBA8UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ETC2RGBA8UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ETC2RGBA8UnormSrgb

wGPUTextureFormat_EACR11Unorm :: WGPUTextureFormat
wGPUTextureFormat_EACR11Unorm = WGPUTextureFormat #const WGPUTextureFormat_EACR11Unorm

wGPUTextureFormat_EACR11Snorm :: WGPUTextureFormat
wGPUTextureFormat_EACR11Snorm = WGPUTextureFormat #const WGPUTextureFormat_EACR11Snorm

wGPUTextureFormat_EACRG11Unorm :: WGPUTextureFormat
wGPUTextureFormat_EACRG11Unorm = WGPUTextureFormat #const WGPUTextureFormat_EACRG11Unorm

wGPUTextureFormat_EACRG11Snorm :: WGPUTextureFormat
wGPUTextureFormat_EACRG11Snorm = WGPUTextureFormat #const WGPUTextureFormat_EACRG11Snorm

wGPUTextureFormat_ASTC4x4Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC4x4Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC4x4Unorm

wGPUTextureFormat_ASTC4x4UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC4x4UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC4x4UnormSrgb

wGPUTextureFormat_ASTC5x4Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC5x4Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC5x4Unorm

wGPUTextureFormat_ASTC5x4UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC5x4UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC5x4UnormSrgb

wGPUTextureFormat_ASTC5x5Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC5x5Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC5x5Unorm

wGPUTextureFormat_ASTC5x5UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC5x5UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC5x5UnormSrgb

wGPUTextureFormat_ASTC6x5Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC6x5Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC6x5Unorm

wGPUTextureFormat_ASTC6x5UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC6x5UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC6x5UnormSrgb

wGPUTextureFormat_ASTC6x6Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC6x6Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC6x6Unorm

wGPUTextureFormat_ASTC6x6UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC6x6UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC6x6UnormSrgb

wGPUTextureFormat_ASTC8x5Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC8x5Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC8x5Unorm

wGPUTextureFormat_ASTC8x5UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC8x5UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC8x5UnormSrgb

wGPUTextureFormat_ASTC8x6Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC8x6Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC8x6Unorm

wGPUTextureFormat_ASTC8x6UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC8x6UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC8x6UnormSrgb

wGPUTextureFormat_ASTC8x8Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC8x8Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC8x8Unorm

wGPUTextureFormat_ASTC8x8UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC8x8UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC8x8UnormSrgb

wGPUTextureFormat_ASTC10x5Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x5Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x5Unorm

wGPUTextureFormat_ASTC10x5UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x5UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x5UnormSrgb

wGPUTextureFormat_ASTC10x6Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x6Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x6Unorm

wGPUTextureFormat_ASTC10x6UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x6UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x6UnormSrgb

wGPUTextureFormat_ASTC10x8Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x8Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x8Unorm

wGPUTextureFormat_ASTC10x8UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x8UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x8UnormSrgb

wGPUTextureFormat_ASTC10x10Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x10Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x10Unorm

wGPUTextureFormat_ASTC10x10UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC10x10UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC10x10UnormSrgb

wGPUTextureFormat_ASTC12x10Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC12x10Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC12x10Unorm

wGPUTextureFormat_ASTC12x10UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC12x10UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC12x10UnormSrgb

wGPUTextureFormat_ASTC12x12Unorm :: WGPUTextureFormat
wGPUTextureFormat_ASTC12x12Unorm = WGPUTextureFormat #const WGPUTextureFormat_ASTC12x12Unorm

wGPUTextureFormat_ASTC12x12UnormSrgb :: WGPUTextureFormat
wGPUTextureFormat_ASTC12x12UnormSrgb = WGPUTextureFormat #const WGPUTextureFormat_ASTC12x12UnormSrgb

wGPUTextureFormat_Force32 :: WGPUTextureFormat
wGPUTextureFormat_Force32 = WGPUTextureFormat #const WGPUTextureFormat_Force32

newtype WGPUTextureUsage = WGPUTextureUsage WGPUFlags deriving newtype (Show, Eq, Storable, Num)

wGPUTextureUsage_None :: WGPUTextureUsage
wGPUTextureUsage_None =  WGPUTextureUsage $ WGPUFlags #const WGPUTextureUsage_None

wGPUTextureUsage_CopySrc :: WGPUTextureUsage
wGPUTextureUsage_CopySrc =  WGPUTextureUsage $ WGPUFlags #const WGPUTextureUsage_CopySrc

wGPUTextureUsage_CopyDst :: WGPUTextureUsage
wGPUTextureUsage_CopyDst =  WGPUTextureUsage $ WGPUFlags #const WGPUTextureUsage_CopyDst

wGPUTextureUsage_TextureBinding :: WGPUTextureUsage
wGPUTextureUsage_TextureBinding =  WGPUTextureUsage $ WGPUFlags #const WGPUTextureUsage_TextureBinding

wGPUTextureUsage_StorageBinding :: WGPUTextureUsage
wGPUTextureUsage_StorageBinding =  WGPUTextureUsage $ WGPUFlags #const WGPUTextureUsage_StorageBinding

wGPUTextureUsage_RenderAttachment :: WGPUTextureUsage
wGPUTextureUsage_RenderAttachment =  WGPUTextureUsage $ WGPUFlags #const WGPUTextureUsage_RenderAttachment

wGPUTextureUsage_TransientAttachment :: WGPUTextureUsage
wGPUTextureUsage_TransientAttachment =  WGPUTextureUsage $ WGPUFlags #const WGPUTextureUsage_TransientAttachment


newtype WGPUPresentMode = WGPUPresentMode CUInt deriving (Eq, Ord, Show)

instance Storable WGPUPresentMode where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUPresentMode x
  poke ptr (WGPUPresentMode x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUPresentMode_Undefined :: WGPUPresentMode
wGPUPresentMode_Undefined = WGPUPresentMode #const WGPUPresentMode_Undefined

wGPUPresentMode_Fifo :: WGPUPresentMode
wGPUPresentMode_Fifo = WGPUPresentMode #const WGPUPresentMode_Fifo

wGPUPresentMode_FifoRelaxed :: WGPUPresentMode
wGPUPresentMode_FifoRelaxed = WGPUPresentMode #const WGPUPresentMode_FifoRelaxed

wGPUPresentMode_Immediate :: WGPUPresentMode
wGPUPresentMode_Immediate = WGPUPresentMode #const WGPUPresentMode_Immediate

wGPUPresentMode_Mailbox :: WGPUPresentMode
wGPUPresentMode_Mailbox = WGPUPresentMode #const WGPUPresentMode_Mailbox

wGPUPresentMode_Force32 :: WGPUPresentMode
wGPUPresentMode_Force32 = WGPUPresentMode #const WGPUPresentMode_Force32


newtype WGPUCompositeAlphaMode = WGPUCompositeAlphaMode CUInt deriving (Eq, Ord, Show)

instance Storable WGPUCompositeAlphaMode where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUCompositeAlphaMode x
  poke ptr (WGPUCompositeAlphaMode x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUCompositeAlphaMode_Auto :: WGPUCompositeAlphaMode
wGPUCompositeAlphaMode_Auto = WGPUCompositeAlphaMode #const WGPUCompositeAlphaMode_Auto

wGPUCompositeAlphaMode_Opaque :: WGPUCompositeAlphaMode
wGPUCompositeAlphaMode_Opaque = WGPUCompositeAlphaMode #const WGPUCompositeAlphaMode_Opaque

wGPUCompositeAlphaMode_Premultiplied :: WGPUCompositeAlphaMode
wGPUCompositeAlphaMode_Premultiplied = WGPUCompositeAlphaMode #const WGPUCompositeAlphaMode_Premultiplied

wGPUCompositeAlphaMode_Unpremultiplied :: WGPUCompositeAlphaMode
wGPUCompositeAlphaMode_Unpremultiplied = WGPUCompositeAlphaMode #const WGPUCompositeAlphaMode_Unpremultiplied

wGPUCompositeAlphaMode_Inherit :: WGPUCompositeAlphaMode
wGPUCompositeAlphaMode_Inherit = WGPUCompositeAlphaMode #const WGPUCompositeAlphaMode_Inherit

wGPUCompositeAlphaMode_Force32 :: WGPUCompositeAlphaMode
wGPUCompositeAlphaMode_Force32 = WGPUCompositeAlphaMode #const WGPUCompositeAlphaMode_Force32

newtype WGPUPrimitiveTopology = WGPUPrimitiveTopology CUInt deriving (Eq, Ord, Show)

instance Storable WGPUPrimitiveTopology where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUPrimitiveTopology x
  poke ptr (WGPUPrimitiveTopology x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUPrimitiveTopology_Undefined :: WGPUPrimitiveTopology
wGPUPrimitiveTopology_Undefined = WGPUPrimitiveTopology #const WGPUPrimitiveTopology_Undefined

wGPUPrimitiveTopology_PointList :: WGPUPrimitiveTopology
wGPUPrimitiveTopology_PointList = WGPUPrimitiveTopology #const WGPUPrimitiveTopology_PointList

wGPUPrimitiveTopology_LineList :: WGPUPrimitiveTopology
wGPUPrimitiveTopology_LineList = WGPUPrimitiveTopology #const WGPUPrimitiveTopology_LineList

wGPUPrimitiveTopology_LineStrip :: WGPUPrimitiveTopology
wGPUPrimitiveTopology_LineStrip = WGPUPrimitiveTopology #const WGPUPrimitiveTopology_LineStrip

wGPUPrimitiveTopology_TriangleList :: WGPUPrimitiveTopology
wGPUPrimitiveTopology_TriangleList = WGPUPrimitiveTopology #const WGPUPrimitiveTopology_TriangleList

wGPUPrimitiveTopology_TriangleStrip :: WGPUPrimitiveTopology
wGPUPrimitiveTopology_TriangleStrip = WGPUPrimitiveTopology #const WGPUPrimitiveTopology_TriangleStrip

wGPUPrimitiveTopology_Force32 :: WGPUPrimitiveTopology
wGPUPrimitiveTopology_Force32 = WGPUPrimitiveTopology #const WGPUPrimitiveTopology_Force32

newtype WGPUIndexFormat = WGPUIndexFormat CUInt deriving (Eq, Ord, Show)

instance Storable WGPUIndexFormat where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUIndexFormat x
  poke ptr (WGPUIndexFormat x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUIndexFormat_Undefined :: WGPUIndexFormat
wGPUIndexFormat_Undefined = WGPUIndexFormat #const WGPUIndexFormat_Undefined

wGPUIndexFormat_Uint16 :: WGPUIndexFormat
wGPUIndexFormat_Uint16 = WGPUIndexFormat #const WGPUIndexFormat_Uint16

wGPUIndexFormat_Uint32 :: WGPUIndexFormat
wGPUIndexFormat_Uint32 = WGPUIndexFormat #const WGPUIndexFormat_Uint32

wGPUIndexFormat_Force32 :: WGPUIndexFormat
wGPUIndexFormat_Force32 = WGPUIndexFormat #const WGPUIndexFormat_Force32


newtype WGPUFilterMode = WGPUFilterMode CUInt deriving (Eq, Ord, Show)

instance Storable WGPUFilterMode where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUFilterMode x
  poke ptr (WGPUFilterMode x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUFilterMode_Undefined :: WGPUFilterMode
wGPUFilterMode_Undefined = WGPUFilterMode #const WGPUFilterMode_Undefined

wGPUFilterMode_Nearest :: WGPUFilterMode
wGPUFilterMode_Nearest = WGPUFilterMode #const WGPUFilterMode_Nearest

wGPUFilterMode_Linear :: WGPUFilterMode
wGPUFilterMode_Linear = WGPUFilterMode #const WGPUFilterMode_Linear

wGPUFilterMode_Force32 :: WGPUFilterMode
wGPUFilterMode_Force32 = WGPUFilterMode #const WGPUFilterMode_Force32


newtype WGPUFrontFace = WGPUFrontFace CUInt deriving (Eq, Ord, Show)

instance Storable WGPUFrontFace where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUFrontFace x
  poke ptr (WGPUFrontFace x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUFrontFace_Undefined :: WGPUFrontFace
wGPUFrontFace_Undefined = WGPUFrontFace #const WGPUFrontFace_Undefined

wGPUFrontFace_CCW :: WGPUFrontFace
wGPUFrontFace_CCW = WGPUFrontFace #const WGPUFrontFace_CCW

wGPUFrontFace_CW :: WGPUFrontFace
wGPUFrontFace_CW = WGPUFrontFace #const WGPUFrontFace_CW

wGPUFrontFace_Force32 :: WGPUFrontFace
wGPUFrontFace_Force32 = WGPUFrontFace #const WGPUFrontFace_Force32


newtype WGPUCullMode = WGPUCullMode CUInt deriving (Eq, Ord, Show)

instance Storable WGPUCullMode where
  alignment _ = alignment (undefined :: CUInt)
  sizeOf _ = sizeOf (undefined :: CUInt)
  peek ptr = do
    x <- peek (castPtr ptr)
    pure $ WGPUCullMode x
  poke ptr (WGPUCullMode x) = do
    let a :: Ptr CUInt = castPtr ptr
    poke a x

wGPUCullMode_Undefined :: WGPUCullMode
wGPUCullMode_Undefined = WGPUCullMode #const WGPUCullMode_Undefined

wGPUCullMode_None :: WGPUCullMode
wGPUCullMode_None = WGPUCullMode #const WGPUCullMode_None

wGPUCullMode_Front :: WGPUCullMode
wGPUCullMode_Front = WGPUCullMode #const WGPUCullMode_Front

wGPUCullMode_Back :: WGPUCullMode
wGPUCullMode_Back = WGPUCullMode #const WGPUCullMode_Back

wGPUCullMode_Force32 :: WGPUCullMode
wGPUCullMode_Force32 = WGPUCullMode #const WGPUCullMode_Force32

newtype WGPUColorWriteMask = WGPUColorWriteMask WGPUFlags deriving newtype (Show, Eq, Storable, Num)

wGPUColorWriteMask_None :: WGPUColorWriteMask
wGPUColorWriteMask_None = WGPUColorWriteMask $ WGPUFlags #const WGPUColorWriteMask_None

wGPUColorWriteMask_Red :: WGPUColorWriteMask
wGPUColorWriteMask_Red = WGPUColorWriteMask $ WGPUFlags #const WGPUColorWriteMask_Red

wGPUColorWriteMask_Green :: WGPUColorWriteMask
wGPUColorWriteMask_Green = WGPUColorWriteMask $ WGPUFlags #const WGPUColorWriteMask_Green

wGPUColorWriteMask_Blue :: WGPUColorWriteMask
wGPUColorWriteMask_Blue = WGPUColorWriteMask $ WGPUFlags #const WGPUColorWriteMask_Blue

wGPUColorWriteMask_Alpha :: WGPUColorWriteMask
wGPUColorWriteMask_Alpha = WGPUColorWriteMask $ WGPUFlags #const WGPUColorWriteMask_Alpha

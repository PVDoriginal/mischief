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

#include "webgpu.h"

module Mischief.WGPU.Enums where

import Foreign 
import Foreign.C.ConstPtr
import Foreign.C.Types
import Data.Void

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


wGPUSType_SurfaceSourceXlibWindow :: WGPUSType
wGPUSType_SurfaceSourceXlibWindow = WGPUSType #const WGPUSType_SurfaceSourceXlibWindow 



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

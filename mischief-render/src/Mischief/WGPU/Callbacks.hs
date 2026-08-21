module Mischief.WGPU.Callbacks where

import Data.Primitive
import Foreign (FunPtr, Ptr)
import Foreign.C
import Foreign.C.ConstPtr
import Foreign.C.Types
import Mischief.WGPU.Opaque
import Mischief.WGPU.Types.Enums

newtype Test = Test (# ByteArray# #)

type WGPURequestAdapterCallback =
  WGPURequestAdapterStatus ->
  Ptr WGPUAdapter ->
  -- message
  ConstPtr CChar ->
  CInt ->
  -- userdata1
  Ptr () ->
  -- userdata2
  Ptr () ->
  IO ()

foreign import ccall "wrapper" requestAdapterCallback :: WGPURequestAdapterCallback -> IO (FunPtr WGPURequestAdapterCallback)

type WGPURequestDeviceCallback =
  WGPURequestDeviceStatus ->
  Ptr WGPUDevice ->
  -- message
  ConstPtr CChar ->
  CInt ->
  -- userdata1
  Ptr () ->
  -- userdata2
  Ptr () ->
  IO ()

foreign import ccall "wrapper" requestDeviceCallback :: WGPURequestDeviceCallback -> IO (FunPtr WGPURequestDeviceCallback)
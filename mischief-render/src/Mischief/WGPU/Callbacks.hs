module Mischief.WGPU.Callbacks where

import Data.Primitive
import Foreign (FunPtr, Ptr)
import Foreign.C
import Foreign.C.ConstPtr
import Foreign.C.Types
import Mischief.WGPU.Enums
import Mischief.WGPU.Opaque

newtype Test = Test (# ByteArray# #)

type WGPURequestAdapterCallback =
  WGPURequestAdapterStatus ->
  Ptr WGPUDevice ->
  -- message
  ConstPtr CChar ->
  CInt ->
  Ptr () ->
  Ptr () ->
  IO ()

foreign import ccall "wrapper" requestAdapterCallback :: WGPURequestAdapterCallback -> IO (FunPtr WGPURequestAdapterCallback)

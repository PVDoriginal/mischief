#include "webgpu.h"

module Mischief.WGPU.Types where 

import Foreign 
import Foreign.C.ConstPtr
import Foreign.C.Types
import Data.Void

import Mischief.WGPU.Callbacks
import Mischief.WGPU.Enums


data WGPUSurface = WGPUSurface

data WGPUChainedStruct = WGPUChainedStruct
  { next :: Ptr WGPUChainedStruct,
    sType :: WGPUSType
  }
  

instance Storable WGPUChainedStruct where 
  alignment _ = #{alignment WGPUChainedStruct}
  sizeOf _ = #{size WGPUChainedStruct}
  peek ptr = do 
    next <- #{peek WGPUChainedStruct, next} ptr 
    sType <- #{peek WGPUChainedStruct, sType} ptr 
    return WGPUChainedStruct {next, sType}
  poke ptr WGPUChainedStruct {next, sType} = do 
    #{poke WGPUChainedStruct, next} ptr next
    #{poke WGPUChainedStruct, sType} ptr sType  


data WGPUSurfaceSourceXlibWindow = WGPUSurfaceSourceXlibWindow
  { chain :: WGPUChainedStruct,
    display :: Ptr Void,
    window :: Int64
  }

instance Storable WGPUSurfaceSourceXlibWindow where 
  alignment _ = #{alignment WGPUSurfaceSourceXlibWindow}
  sizeOf _ = #{size WGPUSurfaceSourceXlibWindow}
  peek ptr = do 
    chain <- #{peek WGPUSurfaceSourceXlibWindow, chain} ptr 
    display <- #{peek WGPUSurfaceSourceXlibWindow, display} ptr 
    window <- #{peek WGPUSurfaceSourceXlibWindow, window} ptr 
    return WGPUSurfaceSourceXlibWindow {chain, display, window}
  poke ptr WGPUSurfaceSourceXlibWindow {chain, display, window} = do 
    #{poke WGPUSurfaceSourceXlibWindow, chain} ptr chain
    #{poke WGPUSurfaceSourceXlibWindow, display} ptr display
    #{poke WGPUSurfaceSourceXlibWindow, window} ptr window
    
data WGPUStringView = WGPUStringView {
  _data :: ConstPtr CChar, 
  length :: Int 
}

instance Storable WGPUStringView where 
  alignment _ = #{alignment WGPUStringView}
  sizeOf _ = #{size WGPUStringView}
  peek ptr = do 
    _data <- #{peek WGPUStringView, data} ptr 
    length <- #{peek WGPUStringView, length} ptr 
    return WGPUStringView {_data, length} 
  poke ptr WGPUStringView {_data, length} = do 
    #{poke WGPUStringView, data} ptr _data
    #{poke WGPUStringView, length} ptr length
  
data WGPUSurfaceDescriptor = WGPUSurfaceDescriptor {
  nextInChain :: Ptr WGPUChainedStruct,
  label :: WGPUStringView 
}

instance Storable WGPUSurfaceDescriptor where 
  alignment _ = #{alignment WGPUSurfaceDescriptor}
  sizeOf _ = #{size WGPUSurfaceDescriptor}
  peek ptr = do 
    nextInChain <- #{peek WGPUSurfaceDescriptor, nextInChain} ptr 
    label <- #{peek WGPUSurfaceDescriptor, label} ptr 
    return WGPUSurfaceDescriptor {nextInChain, label}
  poke ptr WGPUSurfaceDescriptor {nextInChain, label} = do
    #{poke WGPUSurfaceDescriptor, nextInChain} ptr nextInChain
    #{poke WGPUSurfaceDescriptor, label} ptr label 


data WGPURequestAdapterOptions = WGPURequestAdapterOptions {
  nextInChain :: Ptr WGPUChainedStruct, 
  featureLevel :: WGPUFeatureLevel, 
  powerPreference :: WGPUPowerPreference, 
  forceFallbackAdapter :: WGPUBool,
  backendType :: WGPUBackendType, 
  compatibleSurface :: Ptr WGPUSurface 
}

instance Storable WGPURequestAdapterOptions where 
  alignment _ = #{alignment WGPURequestAdapterOptions}
  sizeOf _ = #{size WGPURequestAdapterOptions}
  peek ptr = do 
    nextInChain <- #{peek WGPURequestAdapterOptions, nextInChain} ptr 
    featureLevel <- #{peek WGPURequestAdapterOptions, featureLevel} ptr 
    powerPreference <- #{peek WGPURequestAdapterOptions, powerPreference} ptr 
    forceFallbackAdapter <- #{peek WGPURequestAdapterOptions, forceFallbackAdapter} ptr 
    backendType <- #{peek WGPURequestAdapterOptions, backendType} ptr 
    compatibleSurface <- #{peek WGPURequestAdapterOptions, compatibleSurface} ptr 
    return WGPURequestAdapterOptions {nextInChain, featureLevel, powerPreference, forceFallbackAdapter, backendType, compatibleSurface}
  poke ptr WGPURequestAdapterOptions {nextInChain, featureLevel, powerPreference, forceFallbackAdapter, backendType, compatibleSurface} = do
    #{poke WGPURequestAdapterOptions, nextInChain} ptr nextInChain 
    #{poke WGPURequestAdapterOptions, featureLevel} ptr featureLevel 
    #{poke WGPURequestAdapterOptions, powerPreference} ptr powerPreference 
    #{poke WGPURequestAdapterOptions, forceFallbackAdapter} ptr forceFallbackAdapter 
    #{poke WGPURequestAdapterOptions, backendType} ptr backendType 
    #{poke WGPURequestAdapterOptions, compatibleSurface} ptr compatibleSurface


data WGPURequestAdapterCallbackInfo = WGPURequestAdapterCallbackInfo {
  nextInChain :: Ptr WGPUChainedStruct, 
  mode :: WGPUCallbackMode, 
  callback :: FunPtr WGPURequestAdapterCallback, 
  userdata1 :: Ptr (), 
  userdata2 :: Ptr ()
}

instance Storable WGPURequestAdapterCallbackInfo where 
  alignment _ = #{alignment WGPURequestAdapterCallbackInfo}
  sizeOf _ = #{size WGPURequestAdapterCallbackInfo}
  peek ptr = do 
    nextInChain <- #{peek WGPURequestAdapterCallbackInfo, nextInChain} ptr 
    mode <- #{peek WGPURequestAdapterCallbackInfo, mode} ptr 
    callback <- #{peek WGPURequestAdapterCallbackInfo, callback} ptr 
    userdata1 <- #{peek WGPURequestAdapterCallbackInfo, userdata1} ptr 
    userdata2 <- #{peek WGPURequestAdapterCallbackInfo, userdata2} ptr 
    return WGPURequestAdapterCallbackInfo {nextInChain, mode, callback, userdata1, userdata2}
  poke ptr WGPURequestAdapterCallbackInfo {nextInChain, mode, callback, userdata1, userdata2} = do 
    #{poke WGPURequestAdapterCallbackInfo, nextInChain} ptr nextInChain
    #{poke WGPURequestAdapterCallbackInfo, mode} ptr mode  
    #{poke WGPURequestAdapterCallbackInfo, callback} ptr callback  
    #{poke WGPURequestAdapterCallbackInfo, userdata1} ptr userdata1  
    #{poke WGPURequestAdapterCallbackInfo, userdata2} ptr userdata2  

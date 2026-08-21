#include "webgpu.h"

module Mischief.WGPU.Types.General where 

import Foreign 
import Foreign.C.ConstPtr
import Foreign.C.Types
import Data.Void

import Mischief.WGPU.Callbacks
import Mischief.WGPU.Types.Enums


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


data WGPURequestCallbackInfo f = WGPURequestCallbackInfo {
  nextInChain :: Ptr WGPUChainedStruct, 
  mode :: WGPUCallbackMode, 
  callback :: FunPtr f, 
  userdata1 :: Ptr (), 
  userdata2 :: Ptr ()
}

instance Storable (WGPURequestCallbackInfo WGPURequestAdapterCallback) where 
  alignment _ = #{alignment WGPURequestAdapterCallbackInfo}
  sizeOf _ = #{size WGPURequestAdapterCallbackInfo}
  peek ptr = do 
    nextInChain <- #{peek WGPURequestAdapterCallbackInfo, nextInChain} ptr 
    mode <- #{peek WGPURequestAdapterCallbackInfo, mode} ptr 
    callback <- #{peek WGPURequestAdapterCallbackInfo, callback} ptr 
    userdata1 <- #{peek WGPURequestAdapterCallbackInfo, userdata1} ptr 
    userdata2 <- #{peek WGPURequestAdapterCallbackInfo, userdata2} ptr 
    return WGPURequestCallbackInfo {nextInChain, mode, callback, userdata1, userdata2}
  poke ptr WGPURequestCallbackInfo {nextInChain, mode, callback, userdata1, userdata2} = do 
    #{poke WGPURequestAdapterCallbackInfo, nextInChain} ptr nextInChain
    #{poke WGPURequestAdapterCallbackInfo, mode} ptr mode  
    #{poke WGPURequestAdapterCallbackInfo, callback} ptr callback  
    #{poke WGPURequestAdapterCallbackInfo, userdata1} ptr userdata1  
    #{poke WGPURequestAdapterCallbackInfo, userdata2} ptr userdata2  


instance Storable (WGPURequestCallbackInfo WGPURequestDeviceCallback) where 
  alignment _ = #{alignment WGPURequestDeviceCallbackInfo}
  sizeOf _ = #{size WGPURequestDeviceCallbackInfo}
  peek ptr = do 
    nextInChain <- #{peek WGPURequestDeviceCallbackInfo, nextInChain} ptr 
    mode <- #{peek WGPURequestDeviceCallbackInfo, mode} ptr 
    callback <- #{peek WGPURequestDeviceCallbackInfo, callback} ptr 
    userdata1 <- #{peek WGPURequestDeviceCallbackInfo, userdata1} ptr 
    userdata2 <- #{peek WGPURequestDeviceCallbackInfo, userdata2} ptr 
    return WGPURequestCallbackInfo {nextInChain, mode, callback, userdata1, userdata2}
  poke ptr WGPURequestCallbackInfo {nextInChain, mode, callback, userdata1, userdata2} = do 
    #{poke WGPURequestDeviceCallbackInfo, nextInChain} ptr nextInChain
    #{poke WGPURequestDeviceCallbackInfo, mode} ptr mode  
    #{poke WGPURequestDeviceCallbackInfo, callback} ptr callback  
    #{poke WGPURequestDeviceCallbackInfo, userdata1} ptr userdata1  
    #{poke WGPURequestDeviceCallbackInfo, userdata2} ptr userdata2  

data WGPUShaderSourceWGSL = WGPUShaderSourceWGSL {
  chain :: WGPUChainedStruct, 
  code :: WGPUStringView
}

instance Storable WGPUShaderSourceWGSL where 
  alignment _ = #{alignment WGPUShaderSourceWGSL}
  sizeOf _ = #{size WGPUShaderSourceWGSL}
  peek ptr = do 
    chain <- #{peek WGPUShaderSourceWGSL, chain} ptr 
    code <- #{peek WGPUShaderSourceWGSL, code} ptr 
    return WGPUShaderSourceWGSL {chain, code}
  poke ptr WGPUShaderSourceWGSL {chain, code} = do 
    #{poke WGPUShaderSourceWGSL, chain} ptr chain 
    #{poke WGPUShaderSourceWGSL, code} ptr code 

data WGPUShaderModuleDescriptor = WGPUShaderModuleDescriptor {
  nextInChain :: Ptr WGPUChainedStruct, 
  label :: WGPUStringView
}

instance Storable WGPUShaderModuleDescriptor where 
  alignment _ = #{alignment WGPUShaderModuleDescriptor}
  sizeOf _ = #{size WGPUShaderModuleDescriptor}
  peek ptr = do 
    nextInChain <- #{peek WGPUShaderModuleDescriptor, nextInChain} ptr 
    label <- #{peek WGPUShaderModuleDescriptor, label} ptr 
    return WGPUShaderModuleDescriptor {nextInChain, label}
  poke ptr WGPUShaderModuleDescriptor {nextInChain, label} = do 
    #{poke WGPUShaderModuleDescriptor, nextInChain} ptr nextInChain 
    #{poke WGPUShaderModuleDescriptor, label} ptr label 

data WGPUSurfaceCapabilities = WGPUSurfaceCapabilities {
  nextInChain :: Ptr WGPUChainedStruct, 
  usages :: WGPUTextureUsage, 
  formatCount :: Int,
  formats :: ConstPtr WGPUTextureFormat, 
  presentModeCount :: Int, 
  presentModes :: ConstPtr WGPUPresentMode, 
  alphaModeCount :: Int, 
  alphaModes :: ConstPtr WGPUCompositeAlphaMode
}

instance Storable WGPUSurfaceCapabilities where 
  alignment _ = #{alignment WGPUSurfaceCapabilities}
  sizeOf _ = #{size WGPUSurfaceCapabilities}
  peek ptr = do 
    nextInChain <- #{peek WGPUSurfaceCapabilities, nextInChain} ptr
    usages <- #{peek WGPUSurfaceCapabilities, usages} ptr 
    formatCount <- #{peek WGPUSurfaceCapabilities, formatCount} ptr
    formats <- #{peek WGPUSurfaceCapabilities, formats} ptr 
    presentModeCount <- #{peek WGPUSurfaceCapabilities, presentModeCount} ptr 
    alphaModeCount <- #{peek WGPUSurfaceCapabilities, alphaModeCount} ptr 
    alphaModes <- #{peek WGPUSurfaceCapabilities, alphaModes} ptr 
    return WGPUSurfaceCapabilities {nextInChain, usages, formatCount, formats, presentModeCount, alphaModeCount, alphaModes}
  poke ptr WGPUSurfaceCapabilities {nextInChain, usages, formatCount, formats, presentModeCount, alphaModeCount, alphaModes} = do 
    #{poke WGPUSurfaceCapabilities, nextInChain} ptr nextInChain 
    #{poke WGPUSurfaceCapabilities, usages} ptr usages 
    #{poke WGPUSurfaceCapabilities, formatCount} ptr formatCount 
    #{poke WGPUSurfaceCapabilities, formats} ptr formats 
    #{poke WGPUSurfaceCapabilities, presentModeCount} ptr presentModeCount
    #{poke WGPUSurfaceCapabilities, alphaModeCount} ptr alphaModeCount
    #{poke WGPUSurfaceCapabilities, alphaModes} ptr alphaModes
  
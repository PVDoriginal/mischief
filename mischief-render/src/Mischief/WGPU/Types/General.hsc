#include "webgpu.h"

module Mischief.WGPU.Types.General where 

import Foreign 
import Foreign.C.ConstPtr
import Foreign.C.Types
import Data.Void

import Mischief.WGPU.Callbacks
import Mischief.WGPU.Types.Enums

import Mischief.WGPU.Opaque


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
  
data WGPUMultisampleState = WGPUMultisampleState {
  nextInChain :: Ptr WGPUChainedStruct, 
  count :: Word32, 
  mask :: Word32, 
  alphaToCoverageEnabled :: WGPUBool 
}

instance Storable WGPUMultisampleState where 
  alignment _ = #{alignment WGPUMultisampleState}
  sizeOf _ = #{size WGPUMultisampleState}
  peek ptr = do 
    nextInChain <- #{peek WGPUMultisampleState, nextInChain} ptr 
    count <- #{peek WGPUMultisampleState, count} ptr 
    mask <- #{peek WGPUMultisampleState, mask} ptr 
    alphaToCoverageEnabled <- #{peek WGPUMultisampleState, alphaToCoverageEnabled} ptr 
    return WGPUMultisampleState {nextInChain, count, mask, alphaToCoverageEnabled}
  poke ptr WGPUMultisampleState {nextInChain, count, mask, alphaToCoverageEnabled} = do 
    #{poke WGPUMultisampleState, nextInChain} ptr nextInChain
    #{poke WGPUMultisampleState, count} ptr count
    #{poke WGPUMultisampleState, mask} ptr mask
    #{poke WGPUMultisampleState, alphaToCoverageEnabled} ptr alphaToCoverageEnabled
    
data WGPUPrimitiveState = WGPUPrimitiveState {
  nextInChain :: Ptr WGPUChainedStruct, 
  topology :: WGPUPrimitiveTopology, 
  stripIndexFormat :: WGPUIndexFormat, 
  frontFace :: WGPUFrontFace, 
  cullMode :: WGPUCullMode, 
  unclippedDepth :: WGPUBool 
}

instance Storable WGPUPrimitiveState where
  alignment _ = #{alignment WGPUPrimitiveState}
  sizeOf _ = #{size WGPUPrimitiveState}
  peek ptr = do
    nextInChain <- #{peek WGPUPrimitiveState, nextInChain} ptr
    topology <- #{peek WGPUPrimitiveState, topology} ptr
    stripIndexFormat <- #{peek WGPUPrimitiveState, stripIndexFormat} ptr
    frontFace <- #{peek WGPUPrimitiveState, frontFace} ptr
    cullMode <- #{peek WGPUPrimitiveState, cullMode} ptr
    unclippedDepth <- #{peek WGPUPrimitiveState, unclippedDepth} ptr
    return WGPUPrimitiveState{nextInChain, topology, stripIndexFormat, frontFace, cullMode, unclippedDepth}
  poke ptr WGPUPrimitiveState{nextInChain, topology, stripIndexFormat, frontFace, cullMode, unclippedDepth} = do
    #{poke WGPUPrimitiveState, nextInChain} ptr nextInChain
    #{poke WGPUPrimitiveState, topology} ptr topology
    #{poke WGPUPrimitiveState, stripIndexFormat} ptr stripIndexFormat
    #{poke WGPUPrimitiveState, frontFace} ptr frontFace
    #{poke WGPUPrimitiveState, cullMode} ptr cullMode
    #{poke WGPUPrimitiveState, unclippedDepth} ptr unclippedDepth

data WGPUConstantEntry = WGPUConstantEntry {
  nextInChain :: Ptr WGPUChainedStruct, 
  key :: WGPUStringView, 
  value :: CDouble
}

instance Storable WGPUConstantEntry where 
  alignment _ = #{alignment WGPUConstantEntry}
  sizeOf _ = #{size WGPUConstantEntry}
  peek ptr = do 
    nextInChain <- #{peek WGPUConstantEntry, nextInChain} ptr 
    key <- #{peek WGPUConstantEntry, key} ptr 
    value <- #{peek WGPUConstantEntry, value} ptr 
    return WGPUConstantEntry {nextInChain, key, value}
  poke ptr WGPUConstantEntry {nextInChain, key, value} = do 
    #{poke WGPUConstantEntry, nextInChain} ptr nextInChain 
    #{poke WGPUConstantEntry, key} ptr key 
    #{poke WGPUConstantEntry, value} ptr value

data WGPUVertexAttribute = WGPUVertexAttribute {
  nextInChain :: Ptr WGPUChainedStruct,
  format :: WGPUVertexFormat, 
  offset :: Word64, 
  shaderLocation :: Word32
}

instance Storable WGPUVertexAttribute where
  alignment _ = #{alignment WGPUVertexAttribute}
  sizeOf _ = #{size WGPUVertexAttribute}
  peek ptr = do
    nextInChain <- #{peek WGPUVertexAttribute, nextInChain} ptr
    format <- #{peek WGPUVertexAttribute, format} ptr
    offset <- #{peek WGPUVertexAttribute, offset} ptr
    shaderLocation <- #{peek WGPUVertexAttribute, shaderLocation} ptr
    return WGPUVertexAttribute{nextInChain, format, offset, shaderLocation}
  poke ptr WGPUVertexAttribute{nextInChain, format, offset, shaderLocation} = do
    #{poke WGPUVertexAttribute, nextInChain} ptr nextInChain
    #{poke WGPUVertexAttribute, format} ptr format
    #{poke WGPUVertexAttribute, offset} ptr offset
    #{poke WGPUVertexAttribute, shaderLocation} ptr shaderLocation


data WGPUVertexBufferLayout = WGPUVertexBufferLayout {
  nextInChain :: Ptr WGPUChainedStruct, 
  stepMode :: WGPUVertexStepMode, 
  arrayStride :: Word64, 
  attributeCount :: Int,
  attributes :: ConstPtr WGPUVertexAttribute
}

instance Storable WGPUVertexBufferLayout where
  alignment _ = #{alignment WGPUVertexBufferLayout}
  sizeOf _ = #{size WGPUVertexBufferLayout}
  peek ptr = do
    nextInChain <- #{peek WGPUVertexBufferLayout, nextInChain} ptr
    stepMode <- #{peek WGPUVertexBufferLayout, stepMode} ptr
    arrayStride <- #{peek WGPUVertexBufferLayout, arrayStride} ptr
    attributeCount <- #{peek WGPUVertexBufferLayout, attributeCount} ptr
    attributes <- #{peek WGPUVertexBufferLayout, attributes} ptr
    return WGPUVertexBufferLayout{nextInChain, stepMode, arrayStride, attributeCount, attributes}
  poke ptr WGPUVertexBufferLayout{nextInChain, stepMode, arrayStride, attributeCount, attributes} = do
    #{poke WGPUVertexBufferLayout, nextInChain} ptr nextInChain
    #{poke WGPUVertexBufferLayout, stepMode} ptr stepMode
    #{poke WGPUVertexBufferLayout, arrayStride} ptr arrayStride
    #{poke WGPUVertexBufferLayout, attributeCount} ptr attributeCount
    #{poke WGPUVertexBufferLayout, attributes} ptr attributes

data WGPUVertexState = WGPUVertexState {
  nextInChain :: Ptr WGPUChainedStruct, 
  _module :: Ptr WGPUShaderModule, 
  entryPoint :: WGPUStringView, 
  constantCount :: Int, 
  constants :: ConstPtr WGPUConstantEntry, 
  bufferCount :: Int, 
  buffers :: ConstPtr WGPUVertexBufferLayout
}

instance Storable WGPUVertexState where
  alignment _ = #{alignment WGPUVertexState}
  sizeOf _ = #{size WGPUVertexState}
  peek ptr = do
    nextInChain <- #{peek WGPUVertexState, nextInChain} ptr
    _module <- #{peek WGPUVertexState, module} ptr
    entryPoint <- #{peek WGPUVertexState, entryPoint} ptr
    constantCount <- #{peek WGPUVertexState, constantCount} ptr
    constants <- #{peek WGPUVertexState, constants} ptr
    bufferCount <- #{peek WGPUVertexState, bufferCount} ptr
    buffers <- #{peek WGPUVertexState, buffers} ptr
    return WGPUVertexState{nextInChain, _module, entryPoint, constantCount, constants, bufferCount, buffers}
  poke ptr WGPUVertexState{nextInChain, _module, entryPoint, constantCount, constants, bufferCount, buffers} = do
    #{poke WGPUVertexState, nextInChain} ptr nextInChain
    #{poke WGPUVertexState, module} ptr _module
    #{poke WGPUVertexState, entryPoint} ptr entryPoint
    #{poke WGPUVertexState, constantCount} ptr constantCount
    #{poke WGPUVertexState, constants} ptr constants
    #{poke WGPUVertexState, bufferCount} ptr bufferCount
    #{poke WGPUVertexState, buffers} ptr buffers


data WGPUBufferBindingLayout = WGPUBufferBindingLayout {
  nextInChain :: Ptr WGPUChainedStruct, 
  _type :: WGPUBufferBindingType, 
  hasDynamicOffset :: WGPUBool, 
  minBindingSize :: Word64
}

instance Storable WGPUBufferBindingLayout where 
  alignment _ = #{alignment WGPUBufferBindingLayout}
  sizeOf _ = #{size WGPUBufferBindingLayout}
  peek ptr = do 
    nextInChain <- #{peek WGPUBufferBindingLayout, nextInChain} ptr 
    _type <- #{peek WGPUBufferBindingLayout, type} ptr
    hasDynamicOffset <- #{peek WGPUBufferBindingLayout, hasDynamicOffset} ptr 
    minBindingSize <- #{peek WGPUBufferBindingLayout, minBindingSize} ptr 
    return WGPUBufferBindingLayout {nextInChain, _type, hasDynamicOffset, minBindingSize} 
  poke ptr WGPUBufferBindingLayout {nextInChain, _type, hasDynamicOffset, minBindingSize} = do 
    #{poke WGPUBufferBindingLayout, nextInChain} ptr nextInChain 
    #{poke WGPUBufferBindingLayout, type} ptr _type
    #{poke WGPUBufferBindingLayout, hasDynamicOffset} ptr hasDynamicOffset
    #{poke WGPUBufferBindingLayout, minBindingSize} ptr minBindingSize

data WGPUSamplerBindingLayout = WGPUSamplerBindingLayout {
  nextInChain :: Ptr WGPUChainedStruct,
  _type :: WGPUBufferBindingType
}

instance Storable WGPUSamplerBindingLayout where 
  alignment _ = #{alignment WGPUSamplerBindingLayout}
  sizeOf _ = #{size WGPUSamplerBindingLayout}
  peek ptr = do 
    nextInChain <- #{peek WGPUSamplerBindingLayout, nextInChain} ptr 
    _type <- #{peek WGPUSamplerBindingLayout, type} ptr 
    return WGPUSamplerBindingLayout {nextInChain, _type}
  poke ptr WGPUSamplerBindingLayout {nextInChain, _type} = do 
    #{poke WGPUSamplerBindingLayout, nextInChain} ptr nextInChain
    #{poke WGPUSamplerBindingLayout, type} ptr _type
  
data WGPUTextureBindingLayout = WGPUTextureBindingLayout {
  nextInChain :: Ptr WGPUChainedStruct, 
  sampleType :: WGPUTextureSampleType, 
  viewDimension :: WGPUTextureViewDimension, 
  multisampled :: WGPUBool
} 

instance Storable WGPUTextureBindingLayout where 
  alignment _ = #{alignment WGPUTextureBindingLayout}
  sizeOf _ = #{size WGPUTextureBindingLayout}
  peek ptr = do 
    nextInChain <- #{peek WGPUTextureBindingLayout, nextInChain} ptr 
    sampleType <- #{peek WGPUTextureBindingLayout, sampleType} ptr
    viewDimension <- #{peek WGPUTextureBindingLayout, viewDimension} ptr
    multisampled <- #{peek WGPUTextureBindingLayout, multisampled} ptr
    return WGPUTextureBindingLayout {nextInChain, sampleType, viewDimension, multisampled}
  poke ptr WGPUTextureBindingLayout {nextInChain, sampleType, viewDimension, multisampled} = do 
    #{poke WGPUTextureBindingLayout, nextInChain} ptr nextInChain
    #{poke WGPUTextureBindingLayout, sampleType} ptr sampleType 
    #{poke WGPUTextureBindingLayout, viewDimension} ptr viewDimension
    #{poke WGPUTextureBindingLayout, multisampled} ptr multisampled

data WGPUStorageTextureBindingLayout = WGPUStorageTextureBindingLayout {
  nextInChain :: Ptr WGPUChainedStruct, 
  access :: WGPUStorageTextureAccess, 
  format :: WGPUTextureFormat, 
  viewDimension :: WGPUTextureViewDimension
}

instance Storable WGPUStorageTextureBindingLayout where 
  alignment _ = #{alignment WGPUStorageTextureBindingLayout}
  sizeOf _ = #{size WGPUStorageTextureBindingLayout}
  peek ptr = do 
    nextInChain <- #{peek WGPUStorageTextureBindingLayout, nextInChain} ptr 
    access <- #{peek WGPUStorageTextureBindingLayout, access} ptr
    format <- #{peek WGPUStorageTextureBindingLayout, format} ptr
    viewDimension <- #{peek WGPUStorageTextureBindingLayout, viewDimension} ptr
    return WGPUStorageTextureBindingLayout {nextInChain, access, format, viewDimension}
  poke ptr WGPUStorageTextureBindingLayout {nextInChain, access, format, viewDimension} = do 
    #{poke WGPUStorageTextureBindingLayout, nextInChain} ptr nextInChain
    #{poke WGPUStorageTextureBindingLayout, access} ptr access 
    #{poke WGPUStorageTextureBindingLayout, format} ptr format
    #{poke WGPUStorageTextureBindingLayout, viewDimension} ptr viewDimension

data WGPUBindGroupLayoutEntry = WGPUBindGroupLayoutEntry {
  nextInChain :: Ptr WGPUChainedStruct, 
  binding :: Word32, 
  visibility :: WGPUShaderStage, 
  bindingArraySize :: Word32, 
  buffer :: WGPUBufferBindingLayout, 
  sampler :: WGPUSamplerBindingLayout, 
  texture :: WGPUTextureBindingLayout, 
  storageTexture :: WGPUStorageTextureBindingLayout
}

instance Storable WGPUBindGroupLayoutEntry where 
  alignment _ = #{alignment WGPUStorageTextureBindingLayout}
  sizeOf _ = #{size WGPUStorageTextureBindingLayout}
  peek ptr = do 
    nextInChain <- #{peek WGPUBindGroupLayoutEntry, nextInChain} ptr 
    binding <- #{peek WGPUBindGroupLayoutEntry, binding} ptr
    visibility <- #{peek WGPUBindGroupLayoutEntry, visibility} ptr 
    bindingArraySize <- #{peek WGPUBindGroupLayoutEntry, bindingArraySize} ptr
    buffer <- #{peek WGPUBindGroupLayoutEntry, buffer} ptr
    sampler <- #{peek WGPUBindGroupLayoutEntry, sampler} ptr 
    texture <- #{peek WGPUBindGroupLayoutEntry, texture} ptr 
    storageTexture <- #{peek WGPUBindGroupLayoutEntry, storageTexture} ptr 
    return WGPUBindGroupLayoutEntry {nextInChain, binding, visibility, bindingArraySize, buffer, sampler, texture, storageTexture}
  poke ptr WGPUBindGroupLayoutEntry {nextInChain, binding, visibility, bindingArraySize, buffer, sampler, texture, storageTexture} = do
    #{poke WGPUBindGroupLayoutEntry, nextInChain} ptr nextInChain
    #{poke WGPUBindGroupLayoutEntry, binding} ptr binding
    #{poke WGPUBindGroupLayoutEntry, visibility} ptr visibility
    #{poke WGPUBindGroupLayoutEntry, bindingArraySize} ptr bindingArraySize
    #{poke WGPUBindGroupLayoutEntry, buffer} ptr buffer
    #{poke WGPUBindGroupLayoutEntry, sampler} ptr sampler
    #{poke WGPUBindGroupLayoutEntry, texture} ptr texture
    #{poke WGPUBindGroupLayoutEntry, storageTexture} ptr storageTexture
    
data WGPUPipelineLayoutDescriptor = WGPUPipelineLayoutDescriptor {
  nextInChain :: Ptr WGPUChainedStruct, 
  label :: WGPUStringView, 
  bindGroupLayoutCount :: Word32, 
  bindGroupLayouts :: ConstPtr WGPUBindGroupLayout,
  immediateSize :: CUInt
}

instance Storable WGPUPipelineLayoutDescriptor where 
  alignment _ = #{alignment WGPUStorageTextureBindingLayout}
  sizeOf _ = #{size WGPUStorageTextureBindingLayout}
  peek ptr = do 
    nextInChain <- #{peek WGPUPipelineLayoutDescriptor, nextInChain} ptr
    label <- #{peek WGPUPipelineLayoutDescriptor, label} ptr
    bindGroupLayoutCount <- #{peek WGPUPipelineLayoutDescriptor, bindGroupLayoutCount} ptr
    bindGroupLayouts <- #{peek WGPUPipelineLayoutDescriptor, bindGroupLayouts} ptr
    immediateSize <- #{peek WGPUPipelineLayoutDescriptor, immediateSize} ptr
    return WGPUPipelineLayoutDescriptor {nextInChain, label, bindGroupLayoutCount, bindGroupLayouts, immediateSize}
  poke ptr WGPUPipelineLayoutDescriptor {nextInChain, label, bindGroupLayoutCount, bindGroupLayouts, immediateSize} = do 
    #{poke WGPUPipelineLayoutDescriptor, nextInChain} ptr nextInChain 
    #{poke WGPUPipelineLayoutDescriptor, label} ptr label 
    #{poke WGPUPipelineLayoutDescriptor, bindGroupLayoutCount} ptr bindGroupLayoutCount 
    #{poke WGPUPipelineLayoutDescriptor, bindGroupLayouts} ptr bindGroupLayouts 
    #{poke WGPUPipelineLayoutDescriptor, immediateSize} ptr immediateSize 

data WGPUBlendComponent = WGPUBlendComponent {
  operation :: WGPUBlendOperation, 
  srcFactor :: WGPUBlendFactor, 
  dstFactor :: WGPUBlendFactor
}

instance Storable WGPUBlendComponent where
  alignment _ = #{alignment WGPUBlendComponent}
  sizeOf _ = #{size WGPUBlendComponent}
  peek ptr = do
    operation <- #{peek WGPUBlendComponent, operation} ptr
    srcFactor <- #{peek WGPUBlendComponent, srcFactor} ptr
    dstFactor <- #{peek WGPUBlendComponent, dstFactor} ptr
    return WGPUBlendComponent{operation, srcFactor, dstFactor}
  poke ptr WGPUBlendComponent{operation, srcFactor, dstFactor} = do
    #{poke WGPUBlendComponent, operation} ptr operation
    #{poke WGPUBlendComponent, srcFactor} ptr srcFactor
    #{poke WGPUBlendComponent, dstFactor} ptr dstFactor

data WGPUBlendState = WGPUBlendState {
  color :: WGPUBlendComponent, 
  alpha :: WGPUBlendComponent
}

instance Storable WGPUBlendState where
  alignment _ = #{alignment WGPUBlendState}
  sizeOf _ = #{size WGPUBlendState}
  peek ptr = do
    color <- #{peek WGPUBlendState, color} ptr
    alpha <- #{peek WGPUBlendState, alpha} ptr
    return WGPUBlendState{color, alpha}
  poke ptr WGPUBlendState{color, alpha} = do
    #{poke WGPUBlendState, color} ptr color
    #{poke WGPUBlendState, alpha} ptr alpha

data WGPUColorTargetState = WGPUColorTargetState {
  nextInChain :: Ptr WGPUChainedStruct, 
  format :: WGPUTextureFormat, 
  blend :: ConstPtr WGPUBlendState, 
  writeMask :: WGPUColorWriteMask
}

instance Storable WGPUColorTargetState where
  alignment _ = #{alignment WGPUColorTargetState}
  sizeOf _ = #{size WGPUColorTargetState}
  peek ptr = do
    nextInChain <- #{peek WGPUColorTargetState, nextInChain} ptr
    format <- #{peek WGPUColorTargetState, format} ptr
    blend <- #{peek WGPUColorTargetState, blend} ptr
    writeMask <- #{peek WGPUColorTargetState, writeMask} ptr
    return WGPUColorTargetState{nextInChain, format, blend, writeMask}
  poke ptr WGPUColorTargetState{nextInChain, format, blend, writeMask} = do
    #{poke WGPUColorTargetState, nextInChain} ptr nextInChain
    #{poke WGPUColorTargetState, format} ptr format
    #{poke WGPUColorTargetState, blend} ptr blend
    #{poke WGPUColorTargetState, writeMask} ptr writeMask


data WGPUFragmentState = WGPUFragmentState {
  nextInChain :: Ptr WGPUChainedStruct, 
  _module :: Ptr WGPUShaderModule, 
  entryPoint :: WGPUStringView, 
  constantCount :: Int, 
  constants :: ConstPtr WGPUConstantEntry, 
  targetCount :: Int, 
  targets :: ConstPtr WGPUColorTargetState
}

instance Storable WGPUFragmentState where
  alignment _ = #{alignment WGPUFragmentState}
  sizeOf _ = #{size WGPUFragmentState}
  peek ptr = do
    nextInChain <- #{peek WGPUFragmentState, nextInChain} ptr
    _module <- #{peek WGPUFragmentState, module} ptr
    entryPoint <- #{peek WGPUFragmentState, entryPoint} ptr
    constantCount <- #{peek WGPUFragmentState, constantCount} ptr
    constants <- #{peek WGPUFragmentState, constants} ptr
    targetCount <- #{peek WGPUFragmentState, targetCount} ptr
    targets <- #{peek WGPUFragmentState, targets} ptr
    return WGPUFragmentState{nextInChain, _module, entryPoint, constantCount, constants, targetCount, targets}
  poke ptr WGPUFragmentState{nextInChain, _module, entryPoint, constantCount, constants, targetCount, targets} = do
    #{poke WGPUFragmentState, nextInChain} ptr nextInChain
    #{poke WGPUFragmentState, module} ptr _module
    #{poke WGPUFragmentState, entryPoint} ptr entryPoint
    #{poke WGPUFragmentState, constantCount} ptr constantCount
    #{poke WGPUFragmentState, constants} ptr constants
    #{poke WGPUFragmentState, targetCount} ptr targetCount
    #{poke WGPUFragmentState, targets} ptr targets

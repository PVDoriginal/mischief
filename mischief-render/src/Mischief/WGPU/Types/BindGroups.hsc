#include "webgpu.h"

module Mischief.WGPU.Types.BindGroups where 

import Foreign 
import Foreign.C.ConstPtr
import Foreign.C.Types
import Data.Void

import Mischief.WGPU.Callbacks
import Mischief.WGPU.Types.Enums
import Mischief.WGPU.Types.General

import Mischief.WGPU.Opaque

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
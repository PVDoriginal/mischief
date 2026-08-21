#include "wrapper.h"

WGPUFuture hs_wgpuAdapterRequestDevice(
    WGPUAdapter adapter,
    WGPUDeviceDescriptor const *descriptor,
    WGPURequestDeviceCallbackInfo const *callbackInfo
) {
    return wgpuAdapterRequestDevice(adapter, descriptor, *callbackInfo);
}

WGPUFuture hs_wgpuInstanceRequestAdapter(
  WGPUInstance instance, 
  WGPU_NULLABLE WGPURequestAdapterOptions const * options, 
  WGPURequestAdapterCallbackInfo *callbackInfo
) {
    return wgpuInstanceRequestAdapter(instance, options, *callbackInfo);
}
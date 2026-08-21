#include <webgpu/webgpu.h>

WGPUFuture hs_wgpuAdapterRequestDevice(
    WGPUAdapter adapter,
    WGPUDeviceDescriptor const *descriptor,
    WGPURequestDeviceCallbackInfo const *callbackInfo
);

WGPUFuture hs_wgpuInstanceRequestAdapter(
  WGPUInstance instance, 
  WGPU_NULLABLE WGPURequestAdapterOptions const * options, 
  WGPURequestAdapterCallbackInfo *callbackInfo
);
#include "webgpu.h"

WGPUDevice hs_wgpuAdapterRequestDevice(
    WGPUAdapter adapter
);

WGPUAdapter hs_wgpuInstanceRequestAdapter(
  WGPUInstance instance, 
  WGPUSurface surface
);
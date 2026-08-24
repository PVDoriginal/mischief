#include "wrapper.h"
#include "webgpu.h"
#include <stdio.h>
// #include <unistd.h>

struct adapterBox {
  WGPUAdapter adapter; 
};

struct deviceBox {
  WGPUDevice device; 
};


static void handle_request_adapter(WGPURequestAdapterStatus status,
                                   WGPUAdapter adapter, WGPUStringView message,
                                   void *userdata1, void *userdata2) {

  if (status == WGPURequestAdapterStatus_Success) {
    struct adapterBox *box = userdata1;
    box->adapter = adapter;
  } else {
    printf(" request_adapter status=%#.8x message=%.*s\n", status,
           (int) message.length, message.data);
  }
}

static void handle_request_device(WGPURequestDeviceStatus status,
                                  WGPUDevice device, WGPUStringView message,
                                  void *userdata1, void *userdata2) {
  if (status == WGPURequestDeviceStatus_Success) {
    struct deviceBox *box = userdata1;
    box->device = device;
  } else {
    printf(" request_device status=%#.8x message=%.*s\n", status,
           (int) message.length, message.data);
  }
}

WGPUDevice hs_wgpuAdapterRequestDevice(
    WGPUAdapter adapter
) {
    struct deviceBox deviceBox = {0};

    wgpuAdapterRequestDevice(adapter, NULL, 
                          (const WGPURequestDeviceCallbackInfo){ 
                              .callback = handle_request_device,
                              .userdata1 = &deviceBox
                          });
    // sleep(2);
    return deviceBox.device;
}

WGPUAdapter hs_wgpuInstanceRequestAdapter(
  WGPUInstance instance, 
  WGPUSurface surface
) {
    struct adapterBox adapterBox = {0};

    wgpuInstanceRequestAdapter(instance,
      &(const WGPURequestAdapterOptions){
          .compatibleSurface = surface,
      },
      (const WGPURequestAdapterCallbackInfo){
          .callback = handle_request_adapter,
          .userdata1 = &adapterBox 
      });
    
    // sleep(2);
    return adapterBox.adapter;
}
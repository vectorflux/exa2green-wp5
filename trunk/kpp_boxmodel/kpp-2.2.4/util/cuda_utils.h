/*******************************************************************************
*                                                                              *
*    C CUDA Utility Functions                                                  *
*                                                                              *
*    This header file contains all the required C CUDA funtions used to:       *
*        - Get information about available CUDA enabled GPU(s)                 *
*        - Allocate/Free memory on GPU(s)                                      *
*        - Copy data to/from GPU(s)                                            *
*                                                                              *
*                                                                              *
*    (C) Christos Kannas                                                       *
*    The Cyprus Institute                                                      *
*    E-mail: c.kannas@cyi.ac.cy                                                *
*                                                                              *
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

/** CUDA Libraries */
#include <cuda_runtime_api.h>
#include <cuda_runtime.h>
#include <cuda.h>

/** The maximum number of threads in a block of thread */
#ifndef KPP_CUDA_THREAD_MAX
#define KPP_CUDA_THREAD_MAX 256
#endif

#ifndef KPP_CUDA_WRAP_SIZE
#define KPP_CUDA_WRAP_SIZE 32
#endif

/** Events for timing in CUDA: */
cudaEvent_t cuda_start_event, cuda_stop_event;
/** Set of routine for timing kernel executions */
float cuda_timing_elapsedTime;
/** Start a timer in CUDA */
#define CUDA_timer_start \
    toolTestExec( cudaEventRecord( cuda_start_event, 0 ) )

/** Stop a timer in CUDA */
#define CUDA_timer_stop \
{ \
    toolTestExec( cudaEventRecord( cuda_stop_event, 0 ) ); \
    toolTestExec( cudaEventSynchronize( cuda_stop_event ) ); \
}
/** Elapsed time in CUDA */
#define CUDA_elapsed_time \
{ \
    toolTestExec( cudaEventElapsedTime( &cuda_timing_elapsedTime, \
                                    cuda_start_event, \
                                    cuda_stop_event ) ); \
}

/** Output a debug message à la fprintf */
#define KPP_CUDA_dump_message(...) fprintf(stderr, " KPP_CUDA: " __VA_ARGS__)
  
/** Execute a CUDA command and report error automatically. */
#define toolTestExec(error) checkErrorInline(error, __FILE__, __LINE__)

static inline void checkErrorInline(cudaError_t error, 
                                    const char *currentFile, 
                                    const int currentLine) 
{
    if(cudaSuccess != error) {
        fprintf(stderr,
                "File %s - Line %i - The runtime error is %s\n",
                currentFile,
                currentLine,
                cudaGetErrorString(error));
        exit(-1);
    }
}

/** CUDA Compute Capability Version. */
struct cudacc {
    int major;
    int minor;
};

/** Get the total number of CUDA cores. */
static int computeCoresForDevice(struct cudacc cc, int multiproc) 
{
    if(cc.major == 1) {
        return 8*multiproc;
    } else if (cc.major == 2 && cc.minor == 0) {
        return 32*multiproc;
    } else if (cc.major == 2 && cc.minor == 1) {
        return 48*multiproc;
    } else if (cc.major == 3) {
        return 192*multiproc;
    } else {
        KPP_CUDA_dump_message("Unknown architecture for compute capability %d.%d, assume 32 cores per SM.\n",cc.major,cc.minor);
        return 32*multiproc;
    }
}

/** Print available GPU(s) properties. */
static void displayCudaDevices(int *gpuDevices)
{
    // Get number of CUDA enable devices
    int i, j, nDevices, driverVersion;
    cudaDeviceProp devProp;
    
    toolTestExec(cudaDriverGetVersion(&driverVersion));
    KPP_CUDA_dump_message("CUDA Driver version %d\n", driverVersion);
    toolTestExec(cudaRuntimeGetVersion(&driverVersion));
    KPP_CUDA_dump_message("CUDA Runtime version %d\n\n", driverVersion);
    
    toolTestExec(cudaGetDeviceCount(&nDevices));
    // Return number of GPU devices
    *gpuDevices = nDevices;
    
    // Derived from http://gpucoder.livejournal.com/1064.html
    for(i = 0; i < nDevices; ++i) {
        // Get device properties
        KPP_CUDA_dump_message("*********** CUDA Device #%d ***********\n", i);
        toolTestExec(cudaGetDeviceProperties(&devProp, i));
        KPP_CUDA_dump_message("Major revision number:         %d\n",  devProp.major);
        KPP_CUDA_dump_message("Minor revision number:         %d\n",  devProp.minor);
        KPP_CUDA_dump_message("Name:                          %s\n",  devProp.name);
        KPP_CUDA_dump_message("Number of multiprocessors:     %d\n",  devProp.multiProcessorCount);
        KPP_CUDA_dump_message("Number of cores:               %d\n",  computeCoresForDevice((struct cudacc){devProp.major, devProp.minor}, devProp.multiProcessorCount));
        KPP_CUDA_dump_message("Total global memory:           %zu\n", devProp.totalGlobalMem);
        KPP_CUDA_dump_message("Total shared memory per block: %zu\n", devProp.sharedMemPerBlock);
        KPP_CUDA_dump_message("Total registers per block:     %d\n",  devProp.regsPerBlock);
        KPP_CUDA_dump_message("Warp size:                     %d\n",  devProp.warpSize);
        KPP_CUDA_dump_message("Maximum memory pitch:          %lu\n", devProp.memPitch);
        KPP_CUDA_dump_message("Maximum threads per block:     %d\n",  devProp.maxThreadsPerBlock);
        KPP_CUDA_dump_message("Maximum threads per MP:        %d\n",  devProp.maxThreadsPerMultiProcessor);
        // Maximum Dimension of block
        for (j = 0; j < 3; ++j)
            KPP_CUDA_dump_message("Maximum dimension %d of block:  %d\n", j, devProp.maxThreadsDim[j]);
        // Maximum Dimension of grid
        for (j = 0; j < 3; ++j)
            KPP_CUDA_dump_message("Maximum dimension %d of grid:   %d\n", j, devProp.maxGridSize[j]);
        KPP_CUDA_dump_message("Clock rate:                    %d\n",  devProp.clockRate);
        KPP_CUDA_dump_message("Total constant memory:         %lu\n",  devProp.totalConstMem);
        KPP_CUDA_dump_message("Texture alignment:             %lu\n",  devProp.textureAlignment);
        KPP_CUDA_dump_message("Concurrent copy and execution: %s\n",  (devProp.deviceOverlap ? "Yes" : "No"));
        KPP_CUDA_dump_message("Kernel execution timeout:      %s\n",  (devProp.kernelExecTimeoutEnabled ? "Yes" : "No"));
        KPP_CUDA_dump_message("Unified Memory Addressing:     %s\n\n",  (devProp.unifiedAddressing ? "Yes" : "No"));
    }
}

/** Get Best GPU available.

    Scans the host system for CUDA devices.  
    If multiple devices are found, the "most powerful" device is selected.                                                        
          
    @param[out]    devNumberOut The number of the selected CUDA device     
    @param[out]    propsOut     The properties of the selected CUDA device
 */
void getBestGPUAvailable(int *devNumberOut, cudaDeviceProp *propsOut)
{
    cudaDeviceProp bestProps, props;
    int nDevices = 0;
    int devNumber = 0;
    
    /* Get count of cuda devices */
    toolTestExec(cudaGetDeviceCount(&nDevices));
    KPP_CUDA_dump_message("%d CUDA devices detected.\n", nDevices);
    
    /* Get the first device's properties */
    toolTestExec(cudaGetDeviceProperties(&bestProps, 0));
    
    /* Find the best device */
    for(int i = 1; i < nDevices; ++i) {
        toolTestExec(cudaGetDeviceProperties(&props, i));
        
        /* Filter unsupported devices */
        /*
        if(props.major < 2) {
            printf("Kppa: Ignoring device %d: Compute capability < 2.0 .\n", i);
            continue;
        }
        if(props.warpSize != 32) {
            printf("Kppa: Ignoring device %d: Warp size != 32.\n", i);
            continue;
        }
        */
        /* Select a new best GPU */
        if((props.major > bestProps.major) ||
           (props.major == bestProps.major && props.minor > bestProps.minor) ||
           (props.multiProcessorCount > bestProps.multiProcessorCount) ||
           (props.totalGlobalMem > bestProps.totalGlobalMem) ||
           (props.regsPerBlock > bestProps.regsPerBlock) ||
           (props.sharedMemPerBlock > bestProps.sharedMemPerBlock)) 
        {
            bestProps = props;
            devNumber = i;
        }
    }
    
    /* Report on the selected device */
    KPP_CUDA_dump_message("*********** Selected CUDA Device #%d ***********\n", devNumber);
    KPP_CUDA_dump_message("Major revision number:         %d\n",  bestProps.major);
    KPP_CUDA_dump_message("Minor revision number:         %d\n",  bestProps.minor);
    KPP_CUDA_dump_message("Name:                          %s\n",  bestProps.name);
    KPP_CUDA_dump_message("Number of multiprocessors:     %d\n",  bestProps.multiProcessorCount);
    KPP_CUDA_dump_message("Number of cores:               %d\n",  computeCoresForDevice((struct cudacc){bestProps.major, bestProps.minor}, bestProps.multiProcessorCount));
    KPP_CUDA_dump_message("Total global memory:           %zu\n", bestProps.totalGlobalMem);
    KPP_CUDA_dump_message("Total shared memory per block: %zu\n", bestProps.sharedMemPerBlock);
    KPP_CUDA_dump_message("Total registers per block:     %d\n",  bestProps.regsPerBlock);
    KPP_CUDA_dump_message("Warp size:                     %d\n",  bestProps.warpSize);
    KPP_CUDA_dump_message("Maximum memory pitch:          %lu\n", bestProps.memPitch);
    KPP_CUDA_dump_message("Maximum threads per block:     %d\n",  bestProps.maxThreadsPerBlock);
    // Maximum Dimension of block
    for (int j = 0; j < 3; ++j)
        KPP_CUDA_dump_message("Maximum dimension %d of block:  %d\n", j, bestProps.maxThreadsDim[j]);
    // Maximum Dimension of grid
    for (int j = 0; j < 3; ++j)
        KPP_CUDA_dump_message("Maximum dimension %d of grid:   %d\n", j, bestProps.maxGridSize[j]);
    KPP_CUDA_dump_message("Clock rate:                    %d\n",  bestProps.clockRate);
    KPP_CUDA_dump_message("Total constant memory:         %lu\n",  bestProps.totalConstMem);
    KPP_CUDA_dump_message("Texture alignment:             %lu\n",  bestProps.textureAlignment);
    KPP_CUDA_dump_message("Concurrent copy and execution: %s\n",  (bestProps.deviceOverlap ? "Yes" : "No"));
    KPP_CUDA_dump_message("Kernel execution timeout:      %s\n",  (bestProps.kernelExecTimeoutEnabled ? "Yes" : "No"));
    KPP_CUDA_dump_message("Unified Memory Addressing:     %s\n\n",  (bestProps.unifiedAddressing ? "Yes" : "No"));

    /* Return selected GPU */
    *devNumberOut = devNumber;
    *propsOut = bestProps;
}

/** Initialize Events in Cuda. */
static void InitEvents(void) 
{
    KPP_CUDA_dump_message("CUDA Events created\n");
    toolTestExec( cudaEventCreate( &cuda_start_event ) );
    toolTestExec( cudaEventCreate( &cuda_stop_event ) );
}

/** Free Events in Cuda. */
static void FreeEvents(void) 
{
    KPP_CUDA_dump_message("CUDA Events released\n");
    toolTestExec( cudaEventDestroy( cuda_start_event ) );
    toolTestExec( cudaEventDestroy( cuda_stop_event ) );
}

/** Allocate memory on the GPU in CUDA mode.

    @param[out] address is the address of a variable that is updated by
    this function to contains the address of the allocated memory block

    @param[in] size is the size to allocate in bytes
 */
void gpuMalloc(void **address, size_t size) 
{
    KPP_CUDA_dump_message("CUDA Malloc\n");
    toolTestExec( cudaMalloc( address, size ) );
}

/** Free memory on the GPU in CUDA mode.

    @param[in] address points to a previously allocated memory zone for
    the hardware accelerator
 */
void gpuMemFree(void *address) 
{
    KPP_CUDA_dump_message("CUDA Free\n");
    toolTestExec( cudaFree( address ) );
}

/** Copy from the host to GPU in Cuda mode.

    It's a wrapper around cudaMemcpy*.

    Do not change the place of the pointers in the API. 
    The host address is always before the GPU address...

    @param[in] element_size is the size of one element of the array in byte.

    @param[in] host_address point to the element on the host to read data from.

    @param[out] gpu_address refer to the compact memory area to write data into. 
 */
void copyToGPU(size_t element_size, void const *host_address, void *gpu_address) 
{
    CUDA_timer_start;

    toolTestExec( cudaMemcpy( gpu_address, host_address, element_size, cudaMemcpyHostToDevice) );

    CUDA_timer_stop;
    CUDA_elapsed_time;
    KPP_CUDA_dump_message("Copied %zu bytes of memory from host %p to GPU %p : "
                          "%.1fms - %.2fGB/s\n", element_size, host_address, gpu_address,
                          cuda_timing_elapsedTime, (float)element_size/(cuda_timing_elapsedTime*1000000));
}

/** Copy from the GPU to the host in CUDA mode.

    It's a wrapper around cudaMemcpy*.

    Do not change the place of the pointers in the API. 
    The host address is always first...

    @param[in] element_size is the size of one element of the array in byte.

    @param[out] host_address point to the element on the host to write into.

    @param[in] gpu_address refer to the compact memory area to read data from. 
 */
void copyFromGPU(size_t element_size, void *host_address, void const *gpu_address) 
{
    CUDA_timer_start;

    toolTestExec( cudaMemcpy(host_address, gpu_address, element_size, cudaMemcpyDeviceToHost ) );

    CUDA_timer_stop;
    CUDA_elapsed_time;
    KPP_CUDA_dump_message("Copied %zu bytes of memory from GPU %p to host %p : "
                          "%.1fms - %.2fGB/s\n",element_size, gpu_address,host_address,
                          cuda_timing_elapsedTime,(float)element_size/(cuda_timing_elapsedTime*1000000));
}

/** Copy from GPU to GPU in Cuda mode.

    It's a wrapper around cudaMemcpy*.

    Do not change the place of the pointers in the API. 
    The host address is always before the GPU address...

    @param[in] element_size is the size of one element of the array in byte.

    @param[in] gpu_address point to the element on the gpu to read data from.

    @param[out] gpu_address refer to the compact memory area to write data into. 
 */
void copyGPU(size_t element_size, void const *gpuS_address, void *gpuD_address) 
{
    CUDA_timer_start;

    toolTestExec( cudaMemcpy( gpuD_address, gpuS_address, element_size, cudaMemcpyDeviceToDevice) );

    CUDA_timer_stop;
    CUDA_elapsed_time;
    KPP_CUDA_dump_message("Copied %zu bytes of memory from GPU %p to GPU %p : "
                          "%.1fms - %.2fGB/s\n", element_size, gpuS_address, gpuD_address,
                          cuda_timing_elapsedTime, (float)element_size/(cuda_timing_elapsedTime*1000000));
}

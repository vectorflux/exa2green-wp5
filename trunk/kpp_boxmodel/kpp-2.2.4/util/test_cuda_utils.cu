/****************************************************************************************
*                                                                                       *
*    TestingC CUDA Utility Functions                                                    *
*                                                                                       *
*    This file contains tests for all the C CUDA funtions available in cuda_utils.h:    *
*        - Get information about available CUDA enabled GPU(s)                          *
*        - Create Timing Events                                                         *
*        - Allocate memory on GPU(s)                                                    *
*        - Do some Work in GPU                                                          *
*        - Free memory on GPU(s)                                                        *
*        - Copy data to/from GPU(s)                                                     *
*        - Destroy Timing Events                                                        *
*                                                                                       *
*                                                                                       *
*    (C) Christos Kannas                                                                *
*    The Cyprus Institute                                                               *
*    mailto: c.kannas@cyi.ac.cy                                                         *
*                                                                                       *
****************************************************************************************/
/**
* Testing Functions in cuda_utils.h
*/

#include "cuda_utils.h"

#define N 10

int main() 
{
    /* Device specific variables */
    static int devices = -1;
    static size_t *chunk;      // Memory block required per CUDA device based on memory requirements
    static size_t *chunk32;    // Memory block, rounded to nearest multiple of 32, required per CUDA device
    static size_t *stagesize;  // Total memory in bytes required per CUDA device
    /* Data variables */
    int i;
    //float test_array[N], *d_test_array, final_array[N];
    //size_t size = N*sizeof(float);
    
    /* Solver initialization */
    if(devices == -1) {
        displayCudaDevices(&devices);

        chunk = (size_t *)malloc(sizeof(size_t) * devices);
        chunk32 = (size_t *)malloc(sizeof(size_t) * devices);
        stagesize = (size_t *)malloc(sizeof(size_t) * devices);

        /* Divide equaly across available CUDA devices the required ammount of memory */
        /* Estimate overhead in device memory */
        size_t overhead = N * (sizeof(float) + sizeof(int));
        overhead /= devices;

        /* Estimate cell size in bytes */
        size_t cellsize = sizeof(float) * N;
        cellsize /= devices;

        fprintf(stderr, "overhead: %i\n", overhead);
        fprintf(stderr, "cellsize: %i\n\n", cellsize);

        for(i =0; i < devices; i++){
            cudaDeviceProp props;

            toolTestExec(cudaGetDeviceProperties(&props, i));
            /* Estimate number of cells that will fit in device memory */
            chunk[i] = (props.totalGlobalMem - overhead) / cellsize;
            /* Don't exceed device grid limits */
            size_t maxblocks = props.maxGridSize[0];
            if(chunk[i] > maxblocks) 
                chunk[i] = maxblocks;
            /* Round up to next multiple of 32 */
            chunk32[i] = (chunk[i] + (KPP_CUDA_WRAP_SIZE -1)) & ~(KPP_CUDA_WRAP_SIZE -1);
            /* Total Memory required */
            stagesize[i] = cellsize * chunk32[i];

            fprintf(stderr, "device: %i\n", i);
            fprintf(stderr, "chunk: %i\n", chunk[i]);
            fprintf(stderr, "maxblocks: %i\n", maxblocks);
            fprintf(stderr, "chunk32: %i\n", chunk32[i]);
            fprintf(stderr, "stagesize: %i\n\n", stagesize[i]);
        }
    }
    
    /* Allocate write combined, page-locked host memory */
    float **h_test_array = (float **)malloc(sizeof(float *) * devices);
     /* Allocate device memory for species concentrations */
    float **d_test_array = (float **)malloc(sizeof(float *) * devices);
    //
    for(i = 0; i < devices; i++){
        /* Enable device */
        toolTestExec(cudaSetDevice(i));

        // Initialize Timing Events
        InitEvents();

        /* Allocate CUDA host memory */
        KPP_CUDA_dump_message("Allocating CUDA host memory\n");
        if( cudaSuccess != cudaHostAlloc( &(h_test_array[i]), stagesize[i], cudaHostAllocWriteCombined ) ){
            /* Fall back to page-locked only */
            KPP_CUDA_dump_message("Can't allocate write combined page-locked host memory.\n");
            if( cudaSuccess != cudaMallocHost( &(h_test_array[i]), stagesize[i] ) ){
                /* Fall back to regular malloc */
                KPP_CUDA_dump_message("Can't allocate page-locked host memory.\n");
                h_test_array[i] = (float *)malloc(stagesize[i]);
                if( !(h_test_array[i]) ){
                    KPP_CUDA_dump_message("Failed to allocate host memory.\n");
                    exit(-1);
                }
            }
        }
        /* Allocate CUDA device memory */
        gpuMalloc( (void **) &(d_test_array[i]), stagesize[i] );

        // Destroy Timing Events
        FreeEvents();
    }

    // Init host array
    /*
    for(i = 0; i < N; i++)
        test_array[i] = (float) i*2.5;
    */
    // Initialize Timing Events
    //InitEvents();
    // Allocate GPU Memory
    //gpuMalloc((void **)&d_test_array, size);
    // Copy to GPU
    //copyToGPU(size, test_array, d_test_array);
    // Do some work in GPU
    // Copy from GPU
    //copyFromGPU(size, final_array, d_test_array);
    // Free GPU Memory
    //gpuMemFree(d_test_array);
    // Destroy Timing Events
    //FreeEvents();
    
    /*
    for(i = 0; i < N; i++)
        if(test_array[i] != final_array[i])
            fprintf(stderr, "%i: %f != %f", i, test_array[i], final_array[i]);
    */

    return 0;
}

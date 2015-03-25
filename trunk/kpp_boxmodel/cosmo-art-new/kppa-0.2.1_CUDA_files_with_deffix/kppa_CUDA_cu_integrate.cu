/*------------------- BEGIN kppa_CUDA_cu_integrate.cu BEGIN -------------------*/
/* @file kppa_CUDA_cu_integrate.cu                                             */
/* @author charlesj                                                            */
/* @date 2015-01-22 16:21:35.760333                                            */
/* @brief Interface to time stepping integrator                                */
/*                                                                             */
/* Definitions of interface functions for the Kppa-generated                   */
/* time stepping integrator.  These are the Kppa "entry point" routines.       */
/*                                                                             */
/* This file was generated by Kppa: http://www.paratools.com/Kppa              */
/*-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include "kppa_CUDA_cu_parameters.h"
#include "kppa_CUDA_sparse.h"
#include "kppa_CUDA_cu_integrate.h"



#include "kppa_CUDA_rosenbrock.h"

/*-------------------------------- StageToHost --------------------------------*/
/* Stages concentration data into host-side memory in preparation              */
/* for transfer to device memory.  Data is reorganized to promote              */
/* data access coalescing.                                                     */
/*                                                                             */
/* @param[in]     ncells Number of grid cells                                  */
/* @param[out]    h_conc Species concentrations in page-locked host memory     */
/* @param[in]     pitch  Data pitch (a.k.a stride)                             */
/* @param[in]     conc   Species concentrations                                */
/*-----------------------------------------------------------------------------*/
void StageToHost(size_t const ncells, double* h_conc, size_t const pitch,
    double const  conc[])
{
    for(int i=0; i<ncells; ++i) {
        for(int j=0; j<NSPEC; ++j) {
            h_conc[j*pitch + i] = *(conc++);
        }
    }
}/* END StageToHost */


/*------------------------------- StageFromHost -------------------------------*/
/* Stages concentration data out of host-side memory after it is               */
/* retrieved from the device.  Original data order is restored.                */
/*                                                                             */
/* @param[in]     ncells Number of grid cells                                  */
/* @param[in]     h_conc Species concentrations in page-locked host memory     */
/* @param[in]     pitch  Data pitch (a.k.a stride)                             */
/* @param[out]    conc   Species concentrations                                */
/*-----------------------------------------------------------------------------*/
void StageFromHost(size_t const ncells, double const * h_conc, size_t const
    pitch, double conc[])
{

    for(int i=0; i<ncells; ++i) {
        for(int j=0; j<NSPEC; ++j) {
            *(conc++) = h_conc[j*pitch + i];
        }
    }
}/* END StageFromHost */


/*------------------------------ GetBestDevice -------------------------------*/
/* Scans the host system for CUDA devices of compute capability               */
/* 2.0 or better.  If multiple devices are found, the "most powerful"         */
/* device is selected.                                                        */
/*                                                                            */
/* @param[out]    devNumberOut The number of the selected CUDA device         */
/* @param[out]    propsOut     The properties of the selected CUDA device     */
/* @return        0 on success, or negative value on error                    */
/*----------------------------------------------------------------------------*/
int GetBestDevice(int * devNumberOut, cudaDeviceProp * propsOut)
{
    cudaDeviceProp bestProps, props;
    int devCount = 0;
    int devNumber = 0;

    /* Get count of cuda devices */
    switch(cudaGetDeviceCount(&devCount)) {
    case cudaErrorNoDevice:
        printf("Kppa: No CUDA devices detected.\n");
        return -1;
    case cudaErrorInsufficientDriver:
        printf("Kppa: Could not load CUDA driver.\n");
        return -1;
    case cudaSuccess:
#ifdef SHOW_CUDA_DEVICE_PROPERTIES
        printf("Kppa: %d CUDA devices detected.\n", devCount);
#endif
        break;
    default:
        printf("Kppa: Unknown return value from cudaGetDeviceCount\n");
        return -1;
    }

    /* Get the first device's properties */
    if (cudaGetDeviceProperties(&bestProps, 0) != cudaSuccess) {
        printf("Kppa: Failed to get properties of CUDA device 0.\n");
        return -1;
    }

    /* Find the best device */
    for(int i=1; i<devCount; ++i) {
        if (cudaGetDeviceProperties(&props, i) != cudaSuccess) {
            printf("Kppa: Failed to get properties of CUDA device %d.\n", i);
            continue;
        }
        /* Filter unsupported devices */
        if(props.major < 2) {
            printf("Kppa: Ignoring device %d: Compute capability < 2.0 .\n", i);
            continue;
        }
        if(props.warpSize != 32) {
            printf("Kppa: Ignoring device %d: Warp size != 32.\n", i);
            continue;
        }
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

#ifdef SHOW_CUDA_DEVICE_PROPERTIES
    /* Report on the selected device */
    printf("Kppa: Selected device %d.\n", devNumber);
    printf("Kppa: Device %d: Name:                     %s\n", devNumber, bestProps.name);
    printf("Kppa: Device %d: Version:                  %d.%d\n", devNumber, bestProps.major, bestProps.minor);
    printf("Kppa: Device %d: Warp Size:                %d\n", devNumber, bestProps.warpSize);
    printf("Kppa: Device %d: Clock Rate (MHz):         %d\n", devNumber, bestProps.clockRate/1000);
    printf("Kppa: Device %d: Total Multiprocessors:    %d\n", devNumber, bestProps.multiProcessorCount);
    printf("Kppa: Device %d: Total Global Memory (MB): %ld\n", devNumber, bestProps.totalGlobalMem>>20);
    printf("Kppa: Device %d: Total Const. Memory (KB): %ld\n", devNumber, bestProps.totalConstMem>>10);
    printf("Kppa: Device %d: Block Shared Memory (KB): %ld\n", devNumber, bestProps.sharedMemPerBlock>>10);
    printf("Kppa: Device %d: Block Registers:          %d\n", devNumber, bestProps.regsPerBlock);
    printf("Kppa: Device %d: Max Threads per Block:    %d\n", devNumber, bestProps.maxThreadsPerBlock);
    printf("Kppa: Device %d: Max Memory Pitch (KB):    %ld\n", devNumber, bestProps.memPitch>>10);
    printf("Kppa: Device %d: Max Threadblock Dim:      (%d, %d, %d)\n", devNumber,
            bestProps.maxThreadsDim[0],
            bestProps.maxThreadsDim[1],
            bestProps.maxThreadsDim[2]);
    printf("Kppa: Device %d: Max Grid Size:            (%d, %d, %d)\n", devNumber,
            bestProps.maxGridSize[0],
            bestProps.maxGridSize[1],
            bestProps.maxGridSize[2]);
    printf("Kppa: Device %d: Texture Alignment (B):    %ld\n", devNumber, bestProps.textureAlignment);
    printf("Kppa: Device %d: Supports Device Overlap?: %s\n", devNumber, bestProps.deviceOverlap ? "yes" : "no");
#endif

    *devNumberOut = devNumber;
    *propsOut = bestProps;
    return 0;
} /* END GetBestDevice */

/*------------------------------- GridIntegrate -------------------------------*/
/* Applies the Kppa-generated integrator to the grid                           */
/*                                                                             */
/* @param[in]     ncells Number of grid cells                                  */
/* @param[in,out] conc   Species concentrations                                */
/* @param[in]     tstart Integration start time                                */
/* @param[in]     tend   Integration end time                                  */
/* @param[in]     abstol Absolute integration tolerances for variable species  */
/* @param[in]     reltol Relative integration tolerances for variable species  */
/* @param[in,out] idata  Integer integration in/out parameters                 */
/* @param[in,out] rdata  Real value integration in/out parameters              */
/* @param[out]    lastH  Last timestep in each grid cell                       */
/*-----------------------------------------------------------------------------*/
int GridIntegrate(size_t const ncells, double conc[], double const tstart,
    double const tend, double const  abstol[NVAR], double const
    reltol[NVAR], int idata[20], double rdata[20], double lastH[/* ncells
    */])
{

    #define ABORT(code, fmt, ...) { \
        printf("Kppa: Failure in CUDA integrator: " fmt, ##__VA_ARGS__); \
        idata[19] = code; \
        return code; \
    }


    static int device = -1;
    static size_t chunk = 0;
    static size_t chunk32 = 0;
    static size_t stagesize = 0;
    
    /* Return value */
    int retval = 0;

    /* Solver initialization */
    if(device == -1) {
        /* Select the "best" CUDA device */
        cudaDeviceProp props;
        if(GetBestDevice(&device, &props))
            ABORT(-20, "No suitable CUDA device found.\n");
        if(cudaSetDevice(device) != cudaSuccess)
            ABORT(-20, "Failed to select CUDA device %d.\n", device);

        /* Estimate overhead in device memory */
        size_t overhead = 500*0x100000UL + NVAR*(sizeof(double) + sizeof(int));
    
        /* Estimate cell size in bytes */
        size_t cellsize = sizeof(double) * 
                (NSPEC              // Concentrations
                 + NREACT           // Reaction rates
                 + 11*NVAR          // Integrator tolerances, working data and 6 stages
                 + 3*JAC_LU_NZ);   // Integrator working data
    
        /* Estimate number of cells that will fit in device memory */
        chunk = (props.totalGlobalMem - overhead) / cellsize;

        /* Don't exceed device grid limits */
        size_t maxblocks = (props.maxGridSize[0] / JAC_LU_NZ) * 256;
        if(chunk > maxblocks) chunk = maxblocks;

        /* Don't exceed total number of cells */
        if(chunk > ncells) chunk = ncells;

        /* Round up to next multiple of 32 */
        chunk32 = (chunk + 31) & ~31;
        stagesize = NSPEC*chunk32*sizeof(double);
    }
    
    /* Allocate write combined, page-locked host memory */
      /* Species concentrations in page-locked host memory */
  double* h_conc;

    if(cudaHostAlloc(&h_conc, stagesize, cudaHostAllocWriteCombined) != cudaSuccess) {
        /* Fall back to page-locked only */
        printf("Kppa: Warning: Can't allocate write combined page-locked host memory.\n");
        retval = 1;
        if(cudaMallocHost(&h_conc, stagesize) != cudaSuccess) {
            /* Fall back to regular malloc */
            printf("Kppa: Warning: Can't allocate page-locked host memory.\n");
            if(!(h_conc = (double*)malloc(stagesize)))
                ABORT(-20, "Failed to allocate host memory.\n");
        }
    }
    
    /* Allocate device memory for species concentrations */
      /* Species concentrations in device memory */
  double* d_conc;

    if(cudaMalloc(&d_conc, stagesize) != cudaSuccess)
        ABORT(-20, "Can't allocate device memory.\n");

    for(size_t i=0; i<ncells; i+=chunk) {
        /* Chunk up to the edge of the grid */
        size_t remainder = ncells - i;
        if(remainder < chunk) {
            chunk = remainder;
            chunk32 = (remainder + 31) & ~31;
        }

        /* Stage concentration data onto device */
        StageToHost(chunk, h_conc, chunk32, conc + i*NSPEC);
        if(cudaMemcpy(d_conc, h_conc, stagesize, cudaMemcpyHostToDevice) != cudaSuccess)
            ABORT(-20, "Can't copy concentration data to device\n");

        /* Point to variable and fixed concentrations */
        double * d_var = d_conc;
        double * d_fix = d_conc + NVAR*chunk32;

        /* Invoke the integrator on this block of grid cells */
        Integrate(chunk, d_var, d_fix, i, tstart, tend, abstol, reltol, idata, rdata);

        /* Retrieve concentration data from device */
        if(cudaMemcpy(h_conc, d_conc, stagesize, cudaMemcpyDeviceToHost) != cudaSuccess)
            ABORT(-20, "Can't copy concentration data to device\n");
        StageFromHost(chunk, h_conc, chunk32, conc + i*NSPEC);

        /* Save the last timestep for future use */
        if(lastH) {
            lastH[i] = rdata[11];
        }

        /* Process integrator return code */
        if (idata[19] < 0) {
            printf("Kppa: CELL CHUNK %zd -- INTEGRATION FAILED\n", i);
            for(int j=0; j<20; ++j)
                printf("Kppa: CELL CHUNK %zd, idata[%d] = %d\n", i, j, idata[j]);
            for(int j=0; j<20; ++j)
                printf("Kppa: CELL CHUNK %zd, rdata[%d] = %g\n", i, j, rdata[j]);
            if (idata[19] < retval)
                retval = idata[19];
        } else if (idata[19] > 0) {
            printf("Kppa: CELL CHUNK %zd -- INTEGRATION COMPLETED WITH WARNING\n", i);
            if (retval >= 0 && idata[19] > retval)
                retval = idata[19];
        }
    }

    /* Deallocate memory and return*/
    cudaFree(d_conc);
    cudaFreeHost(h_conc);
    return retval;

}/* END GridIntegrate */



/*---------------------- END kppa_CUDA_cu_integrate.h END ---------------------*/

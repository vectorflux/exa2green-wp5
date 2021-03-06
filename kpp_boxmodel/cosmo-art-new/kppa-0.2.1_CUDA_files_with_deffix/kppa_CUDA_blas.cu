/*----------------------- BEGIN kppa_CUDA_blas.cu BEGIN -----------------------*/
/* @file kppa_CUDA_blas.cu                                                     */
/* @author charlesj                                                            */
/* @date 2015-07-06 14:41:44.580956                                            */
/* @brief Basic linear algebra subprogram definitions                          */
/*                                                                             */
/* A reduced set of BLAS routines optimized for Kppa-generated solvers         */
/*                                                                             */
/* This file was generated by Kppa: http://www.paratools.com/Kppa              */
/*-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include "kppa_CUDA_cu_parameters.h"
#include "kppa_CUDA_blas.h"



/*----------------------------------- WCOPY -----------------------------------*/
/* Copies vector x to vector y: y <= x                                         */
/* Like the BLAS {S,D}COPY(N,X,1,Y,1)                                          */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in]     n        Vector length                                       */
/* @param[in]     d_x      Vector x in device memory                           */
/* @param[out]    d_y      Vector y in device memory                           */
/*-----------------------------------------------------------------------------*/
void WCOPY(size_t const ncells32, size_t const n, double const  d_x[/* n */],
    double d_y[/* n */])
{
    cudaMemcpy(d_y, d_x, ncells32*n*sizeof(double), cudaMemcpyDeviceToDevice);
}/* END WCOPY */



/*---------------------------------- d_WSCAL ----------------------------------*/
/* CUDA kernel for WSCAL.                                                      */
/* Each thread calculates one element of the output array.                     */
/*                                                                             */
/* @param[in]     n     Vector length                                          */
/* @param[in]     alpha Scalar                                                 */
/* @param[in,out] x     Vector x                                               */
/*-----------------------------------------------------------------------------*/
__global__
void d_WSCAL(size_t const n, double const alpha, double x[/* n */])
{
    size_t idx = blockDim.x*blockIdx.x + threadIdx.x;
    if(idx < n) {
        x[idx] *= alpha;
    }
}/* END d_WSCAL */


/*----------------------------------- WSCAL -----------------------------------*/
/* Constant times a vector: x <= alpha*x                                       */
/* Like the BLAS {S,D}SCAL(N,alpha,X,1)                                        */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in]     n        Vector length                                       */
/* @param[in]     alpha    Scalar                                              */
/* @param[in,out] d_x      Vector x in device memory                           */
/*-----------------------------------------------------------------------------*/
void WSCAL(size_t const ncells32, size_t const n, double const alpha, double
    d_x[/* n */])
{
    size_t nBlocks = ((ncells32*n + 255) & ~255) >> 8;
    size_t nThreads = 256;

    if(alpha == 0) {
        cudaMemset(d_x, 0, ncells32*n*sizeof(double));
    } else if(alpha == 1.0) {
        return;
    } else {
        d_WSCAL<<<nBlocks, nThreads>>>(ncells32*n, alpha, d_x);
    }
}/* END WSCAL */



/*---------------------------------- d_WAXPY ----------------------------------*/
/* CUDA kernel for WAXPY.                                                      */
/* Each thread calculates one element of the output array.                     */
/*                                                                             */
/* @param[in]     n     Vector length                                          */
/* @param[in]     alpha Scalar                                                 */
/* @param[in]     x     Vector x                                               */
/* @param[in,out] y     Vector y                                               */
/*-----------------------------------------------------------------------------*/
__global__
void d_WAXPY(size_t const n, double const alpha, double const  x[/* n */],
    double y[/* n */])
{
    size_t idx = blockDim.x*blockIdx.x + threadIdx.x;
    if(idx < n) {
        y[idx] += alpha * x[idx];
    }
}/* END d_WAXPY */


/*----------------------------------- WAXPY -----------------------------------*/
/* Constant times a vector plus a vector: y <= y + alpha*x                     */
/* Like the BLAS {S,D}AXPY(N,alpha,X,1,Y,1)                                    */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in]     n        Vector length                                       */
/* @param[in]     alpha    Scalar                                              */
/* @param[in]     d_x      Vector x in device memory                           */
/* @param[in,out] d_y      Vector y in device memory                           */
/*-----------------------------------------------------------------------------*/
void WAXPY(size_t const ncells32, size_t const n, double const alpha, double
    const  d_x[/* n */], double d_y[/* n */])
{
    size_t nBlocks = ((ncells32*n + 255) & ~255) >> 8;
    size_t nThreads = 256;

    if(alpha == 0) {
        return;
    } else {
        d_WAXPY<<<nBlocks, nThreads>>>(ncells32*n, alpha, d_x, d_y);
    }
}/* END WAXPY */



/*---------------------------------- d_WYMXDA ---------------------------------*/
/* CUDA kernel for WYMXDA.                                                     */
/* Each thread calculates one element of the output array.                     */
/*                                                                             */
/* @param[in]     n     Vector length                                          */
/* @param[in]     x     Vector x                                               */
/* @param[in]     y     Vector y                                               */
/* @param[in]     alpha Scalar                                                 */
/* @param[in,out] z     Vector z                                               */
/*-----------------------------------------------------------------------------*/
__global__
void d_WYMXDA(size_t const n, double const  x[/* n */], double const  y[/* n
    */], double const alpha, double z[/* n */])
{
    size_t idx = blockDim.x*blockIdx.x + threadIdx.x;
    if(idx < n) {
        z[idx] = (y[idx] - x[idx]) / alpha;
    }
}/* END d_WYMXDA */


/*----------------------------------- WYMXDA ----------------------------------*/
/* Difference of two vectors divided by a constant: z <= (y - x) / alpha       */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in]     n        Vector length                                       */
/* @param[in]     d_x      Vector x in device memory                           */
/* @param[in]     d_y      Vector y in device memory                           */
/* @param[in]     alpha    Scalar                                              */
/* @param[out]    d_z      Vector z in device memory                           */
/*-----------------------------------------------------------------------------*/
void WYMXDA(size_t const ncells32, size_t const n, double const  d_x[/* n
    */], double const  d_y[/* n */], double const alpha, double d_z[/* n */])
{
    size_t nBlocks = ((ncells32*n + 255) & ~255) >> 8;
    size_t nThreads = 256;
    d_WYMXDA<<<nBlocks, nThreads>>>(ncells32*n, d_x, d_y, alpha, d_z);
}/* END WYMXDA */


/*-------------------------- END kppa_CUDA_blas.h END -------------------------*/

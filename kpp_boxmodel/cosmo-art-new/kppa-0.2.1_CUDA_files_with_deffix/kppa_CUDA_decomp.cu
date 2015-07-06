/*---------------------- BEGIN kppa_CUDA_decomp.cu BEGIN ----------------------*/
/* @file kppa_CUDA_decomp.cu                                                   */
/* @author charlesj                                                            */
/* @date 2015-07-06 14:41:44.835941                                            */
/* @brief LU decomposition of the row-compressed sparse Jacobian               */
/*                                                                             */
/* LU decomposition of the row-compressed sparse Jacobian                      */
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
#include "kppa_CUDA_decomp.h"


/*---------------------------------- d_Decomp ---------------------------------*/
/* CUDA kernel for Decomp                                                      */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in,out] A        Row-compressed matrix with zero fill                */
/*-----------------------------------------------------------------------------*/
__global__
void d_Decomp(size_t const ncells32, double* A)
{

    size_t idx = blockDim.x*blockIdx.x + threadIdx.x;
    if(idx < ncells32) {
        A += idx;
        for(int i=1; i<NVAR; ++i) {
            for(int j=JAC_LU_CROW[i]; j<JAC_LU_DIAG[i]; ++j) {
                int c = JAC_LU_ICOL[j];
                int d = JAC_LU_DIAG[c];
                double piv = A[j*ncells32] / A[d*ncells32];
                A[j*ncells32] = piv;
                int x = j + 1;
                for(int k=d+1; k<JAC_LU_CROW[c+1]; ++k) {
                    while(JAC_LU_ICOL[x] != JAC_LU_ICOL[k]) ++x;
                    A[x*ncells32] -= piv * A[k*ncells32];
                }
            }
        }
    }
}/* END d_Decomp */



/*----------------------------------- Decomp ----------------------------------*/
/* In-place sparse LU decomposition                                            */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in,out] d_A      Row-compressed matrix with zero fill in device memory */
/*-----------------------------------------------------------------------------*/
int Decomp(size_t const ncells32, double* d_A)
{

    size_t nBlocks = ((ncells32 + 127) & ~127) >> 7;
    size_t nThreads = 128;
    d_Decomp<<<nBlocks, nThreads>>>(ncells32, d_A);
    return 0;
}/* END Decomp */


/*------------------------- END kppa_CUDA_decomp.h END ------------------------*/

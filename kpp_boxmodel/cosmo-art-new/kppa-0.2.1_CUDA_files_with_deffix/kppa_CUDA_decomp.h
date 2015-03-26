/*----------------------- BEGIN kppa_CUDA_decomp.h BEGIN ----------------------*/
/* @file kppa_CUDA_decomp.h                                                    */
/* @author charlesj                                                            */
/* @date 2015-01-22 16:21:36.104577                                            */
/* @brief LU decomposition of the row-compressed sparse Jacobian               */
/*                                                                             */
/* LU decomposition of the row-compressed sparse Jacobian                      */
/*                                                                             */
/* This file was generated by Kppa: http://www.paratools.com/Kppa              */
/*-----------------------------------------------------------------------------*/

#ifndef __KPPA_CUDA_DECOMP_H__
#define __KPPA_CUDA_DECOMP_H__



#ifdef __cplusplus
extern "C" {
#endif


/*----------------------------------- Decomp ----------------------------------*/
/* In-place sparse LU decomposition                                            */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in,out] d_A      Row-compressed matrix with zero fill in device memory */
/*-----------------------------------------------------------------------------*/
int Decomp(size_t const ncells32, double* d_A);



#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __KPPA_CUDA_DECOMP_H__ */
/*------------------------- END kppa_CUDA_decomp.h END ------------------------*/
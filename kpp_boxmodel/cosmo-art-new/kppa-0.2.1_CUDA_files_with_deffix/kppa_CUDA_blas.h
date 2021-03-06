/*------------------------ BEGIN kppa_CUDA_blas.h BEGIN -----------------------*/
/* @file kppa_CUDA_blas.h                                                      */
/* @author charlesj                                                            */
/* @date 2015-07-06 14:41:44.583147                                            */
/* @brief Basic linear algebra subprogram definitions                          */
/*                                                                             */
/* A reduced set of BLAS routines optimized for Kppa-generated solvers         */
/*                                                                             */
/* This file was generated by Kppa: http://www.paratools.com/Kppa              */
/*-----------------------------------------------------------------------------*/

#ifndef __KPPA_CUDA_BLAS_H__
#define __KPPA_CUDA_BLAS_H__



#ifdef __cplusplus
extern "C" {
#endif


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
    double d_y[/* n */]);


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
    d_x[/* n */]);


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
    const  d_x[/* n */], double d_y[/* n */]);


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
    */], double const  d_y[/* n */], double const alpha, double d_z[/* n
    */]);



#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* __KPPA_CUDA_BLAS_H__ */
/*-------------------------- END kppa_CUDA_blas.h END -------------------------*/

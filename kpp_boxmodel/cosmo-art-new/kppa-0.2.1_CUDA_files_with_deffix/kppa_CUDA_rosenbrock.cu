/*-------------------- BEGIN kppa_CUDA_rosenbrock.cu BEGIN --------------------*/
/* @file kppa_CUDA_rosenbrock.cu                                               */
/* @author charlesj                                                            */
/* @date 2015-07-06 14:41:45.062659                                            */
/* @brief Solves the system y' = F(t,y) using a Rosenbrock method              */
/*                                                                             */
/* Solves the system y' = F(t,y) using a Rosenbrock method defined by:         */
/*                                                                             */
/*     G = 1 / (H*gamma) - Jacobian(t0,Y0)                                     */
/*     T_i = t0 + Alpha(i) * H                                                 */
/*     Y_i = Y0 + \sum_{j=1}^{i-1} A(i,j)*K_j                                  */
/*     G * K_i = F(T_i, Y_i) + \sum_{j=1}^S C(i,j)/H * K_j                     */
/*               + gamma(i)*dF/dT(t0, Y0)                                      */
/*     Y1 = Y0 + \sum_{j=1}^S M(j)*K_j                                         */
/*                                                                             */
/* For details on Rosenbrock methods and their implementations:                */
/*     (1) E. Harier and G. Wanner,                                            */
/*         "Solving Ordenary Differential Equations II: stiff and              */
/*         differential-algebraic problems." Computational Mathematics,        */
/*         Springer-Verlag (1996)                                              */
/*     (2) KPP - the Kinetic PreProcessor.                                     */
/*         http://people.cs.vt.edu/~asandu/Software/Kpp/                       */
/*                                                                             */
/* Rosenbrock implementations in both (1) and (2) inspired this code.          */
/* This code presents an interface similar to the KPP implementation           */
/* for compatibility with existing systems.                                    */
/*                                                                             */
/* -- Explanation of integer input parameters:                                 */
/*                                                                             */
/*     idata[0] == 0 : F = F(t,y) Depends on T (non-autonomous).               */
/*              != 0 : F = F(y)   Independent of T (autonomous).               */
/*     idata[1] == 0 : Use all values in tolerance vectors.                    */
/*              != 0 : Use only the first value in the tolerance vectors.      */
/*     idata[2] == 0 : Maximum number of integration steps = 100000.           */
/*              != 0 : Maximum number of integration steps = idata[2].         */
/*     idata[3] == 0 : Method is Ros4.                                         */
/*              == 1 : Method is Ros2.                                         */
/*              == 2 : Method is Ros3.                                         */
/*              == 3 : Method is Ros4.                                         */
/*              == 4 : Method is Rodas3.                                       */
/*              == 5 : Method is Rodas4.                                       */
/*              >= 6 : Error.                                                  */
/*     idata[4] == 0 : Assume tolerance vectors are reasonably valued.         */
/*              != 0 : Check tolerance vectors for unreasonable values.        */
/*                                                                             */
/* -- Explanation of real value input parameters:                              */
/*                                                                             */
/*     rdata[0]: Lower bound on the integration step size.                     */
/*               Default: 0.0                                                  */
/*     rdata[1]: Upper bound on the integration step size.                     */
/*               Default: abs(tend - tstart)                                   */
/*     rdata[2]: Starting value for the integration step size.                 */
/*               Default: minimum step size                                    */
/*     rdata[3]: Lower bound on step decrease factor.                          */
/*               Default: 0.2                                                  */
/*     rdata[4]: Upper bound on step increase factor.                          */
/*               Default: 6.0                                                  */
/*     rdata[5]: Step decrease factor after step rejection.                    */
/*               Default: 0.1                                                  */
/*     rdata[6]: Safety factor in computation of new step size.                */
/*               Default: 0.9                                                  */
/*                                                                             */
/* -- Explanation of integer output parameters:                                */
/*                                                                             */
/*     idata[10]: Number of function evaluations.                              */
/*     idata[11]: Number of Jacobian evaluations.                              */
/*     idata[12]: Number of steps taken.                                       */
/*     idata[13]: Number of accepted steps.                                    */
/*     idata[14]: Number of rejected steps.                                    */
/*     idata[15]: Number of LU decompositions.                                 */
/*     idata[16]: Number of forward/backward substitutions.                    */
/*     idata[17]: Number of singular matrix decompositions.                    */
/*     idata[19]: Integrator exit status.                                      */
/*                Zero indicates success.                                      */
/*                Positive values indicate success with warning.               */
/*                Negative values indicate failure.                            */
/*                                                                             */
/* -- Explanation of real-value output parameters:                             */
/*                                                                             */
/*     rdata[10]: The time corresponding to the computed Y upon return.        */
/*     rdata[11]: The last accepted step before exit.                          */
/*                Use this value as rdata[2] in subsequent runs.               */
/*     rdata[12]: Scaled norm of the error vector on exit.                     */
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
#include "kppa_CUDA_rates.h"
#include "kppa_CUDA_function.h"
#include "kppa_CUDA_decomp.h"
#include "kppa_CUDA_solve.h"
#include "kppa_CUDA_jacobian.h"
#include "kppa_CUDA_sparse.h"
#include "kppa_CUDA_rosenbrock.h"




#include <float.h>

/* Minimum time delta */
#define MIN_DELT ((double)10.0 * DBL_EPSILON)


/* Rosenbrock new step solution coefficients in device constant memory */
__constant__ double d_M[6];

/* Rosenbrock error estimation coefficients in device constant memory */
__constant__ double d_E[6];

/* Rosenbrock coefficient matrix A in device constant memory */
__constant__ double d_A[15];

/* Rosenbrock coefficient matrix C in device constant memory */
__constant__ double d_C[15];



/*----------------------------------------------------------------------------*/
/* A two-stage L-stable method of order 2                                     */
/*                                                                            */
/* E. Harier and G. Wanner, "Solving Ordenary Differential Equations II:      */
/* stiff and differential-algebraic problems." Computational Mathematics,     */
/* Springer-Verlag (1996)                                                     */
/*                                                                            */
/* @param[out] name     Method name                                           */
/* @param[out] nStage   Number of method stages                               */
/* @param[out] invLoEst One divided by the estimation of local order          */
/* @param[out] M        Coefficients for new step solution                    */
/* @param[out] E        Coefficients for error estimation                     */
/* @param[out] A        Lower triangular coefficient matrix                   */
/* @param[out] C        Lower triangular coefficient matrix                   */
/* @param[out] alpha    Y at stage i is approx. Y(T + H*Alpha_i)              */
/* @param[out] gamma    Stage i Gamma = sum(gamma[j])                         */
/* @param[out] F        Function evaluation flags                             */
/*----------------------------------------------------------------------------*/
void InitRos2(char ** name, int * nStage, double * invLoEst, double M[], double E[],
        double A[], double C[], double alpha[], double gamma[], char F[])
{
    *name = "Ros2";

    *nStage = 2;

    *invLoEst = 0.5; /* 1 / 2 */

    M[0] = 0.8786796564403575;
    M[1] = 0.2928932188134525;

    E[0] = 0.2928932188134525;
    E[1] = 0.2928932188134525;

    A[0] = 0.585786437626905;

    C[0] = -1.17157287525381;

    alpha[0] = 0.0;
    alpha[1] = 1.0;

    gamma[0] =  1.7071067811865475;
    gamma[1] = -1.7071067811865475;

    F[0] = 1;
    F[1] = 1;
} /* END InitRos2 */


/*----------------------------------------------------------------------------*/
/* A three-stage L-stable method of order 3                                   */
/*                                                                            */
/* E. Harier and G. Wanner, "Solving Ordenary Differential Equations II:      */
/* stiff and differential-algebraic problems." Computational Mathematics,     */
/* Springer-Verlag (1996)                                                     */
/*                                                                            */
/* @param[out] name     Method name                                           */
/* @param[out] nStage   Number of method stages                               */
/* @param[out] invLoEst One divided by the estimation of local order          */
/* @param[out] M        Coefficients for new step solution                    */
/* @param[out] E        Coefficients for error estimation                     */
/* @param[out] A        Lower triangular coefficient matrix                   */
/* @param[out] C        Lower triangular coefficient matrix                   */
/* @param[out] alpha    Y at stage i is approx. Y(T + H*Alpha_i)              */
/* @param[out] gamma    Stage i Gamma = sum(gamma[j])                         */
/* @param[out] F        Function evaluation flags                             */
/*----------------------------------------------------------------------------*/
void InitRos3(char ** name, int * nStage, double * invLoEst, double M[], double E[],
        double A[], double C[], double alpha[], double gamma[], char F[])
{
    /* Method name */
    *name = "Ros3";

    /* Number of stages */
    *nStage = 3;

    /* Inverse estimation of local order: 1/3 */
    *invLoEst = 0.3333333333333333;

    /* Coefficients for new step solution */
    M[0] = 1.0;
    M[1] = 6.1697947043828245592553615689730;
    M[2] = -0.4277225654321857332623837380651;

    /* Coefficients for error estimation */
    E[0] = 0.5;
    E[1] = -2.9079558716805469821718236208017;
    E[2] = 0.2235406989781156962736090927619;

    /* Lower triangular coefficient matrix A */
    A[0] = 1.0;
    A[1] = 1.0;
    A[2] = 0.0;

    /* Lower triangular coefficient matrix C */
    C[0] = -1.0156171083877702091975600115545;
    C[1] = 4.0759956452537699824805835358067;
    C[2] = 9.2076794298330791242156818474003;

    /* Two function evaluations */
    F[0] = 1;
    F[1] = 1;
    F[2] = 0;

    /* Y_stage_i ~ Y( T + H*Alpha_i ) */
    alpha[0] = 0.0;
    alpha[1] = 0.43586652150845899941601945119356;
    alpha[2] = 0.43586652150845899941601945119356;

    /* Gamma_i = \sum_j  gamma_{i,j}  */
    gamma[0] = 0.43586652150845899941601945119356;
    gamma[1] = 0.24291996454816804366592249683314;
    gamma[2] = 2.1851380027664058511513169485832;

} /* END InitRos3 */


/*----------------------------------------------------------------------------*/
/* A four-stage L-stable method of order 4                                    */
/*                                                                            */
/* E. Harier and G. Wanner, "Solving Ordenary Differential Equations II:      */
/* stiff and differential-algebraic problems." Computational Mathematics,     */
/* Springer-Verlag (1996)                                                     */
/*                                                                            */
/* @param[out] name     Method name                                           */
/* @param[out] nStage   Number of method stages                               */
/* @param[out] invLoEst One divided by the estimation of local order          */
/* @param[out] M        Coefficients for new step solution                    */
/* @param[out] E        Coefficients for error estimation                     */
/* @param[out] A        Lower triangular coefficient matrix                   */
/* @param[out] C        Lower triangular coefficient matrix                   */
/* @param[out] alpha    Y at stage i is approx. Y(T + H*Alpha_i)              */
/* @param[out] gamma    Stage i Gamma = sum(gamma[j])                         */
/* @param[out] F        Function evaluation flags                             */
/*----------------------------------------------------------------------------*/
void InitRos4(char ** name, int * nStage, double * invLoEst, double M[], double E[],
        double A[], double C[], double alpha[], double gamma[], char F[])
{
    /* Method name */
    *name = "Ros4";

    /* Number of stages */
    *nStage = 4;

    /* Inverse estimation of local order: 1/4 */
    *invLoEst = 0.25;

    /* Coefficients for new step solution */
    M[0] = 2.255570073418735;
    M[1] = 0.2870493262186792;
    M[2] = 0.4353179431840180;
    M[3] = 1.093502252409163;

    /* Coefficients for error estimation */
    E[0] = -0.2815431932141155;
    E[1] = -0.07276199124938920;
    E[2] = -0.1082196201495311;
    E[3] = -1.093502252409163;

    /* Lower triangular coefficient matrix A */
    A[0] = 2.0;
    A[1] = 1.867943637803922;
    A[2] = 0.2344449711399156;
    A[3] = 1.867943637803922;
    A[4] = 0.2344449711399156;
    A[5] = 0.0;

    /* Lower triangular coefficient matrix C */
    C[0] = -7.137615036412310;
    C[1] =  2.580708087951457;
    C[2] =  0.6515950076447975;
    C[3] = -2.137148994382534;
    C[4] = -0.3214669691237626;
    C[5] = -0.6949742501781779;

    /* Three function evaluations */
    F[0] = 1;
    F[1] = 1;
    F[2] = 1;
    F[3] = 0;

    /* Y_stage_i ~ Y( T + H*Alpha_i ) */
    alpha[0] = 0.0;
    alpha[1] = 1.145640000000000;
    alpha[2] = 0.6552168638155900;
    alpha[3] = 0.6552168638155900;

    /* Gamma_i = \sum_j  gamma_{i,j}  */
    gamma[0] = 0.5728200000000000;
    gamma[1] = -1.769193891319233;
    gamma[2] = 0.7592633437920482;
    gamma[3] = -0.1049021087100450;

} /* END InitRos4 */


/*----------------------------------------------------------------------------*/
/* A four-stage stiffly-stable method of order 4                              */
/*                                                                            */
/* E. Harier and G. Wanner, "Solving Ordenary Differential Equations II:      */
/* stiff and differential-algebraic problems." Computational Mathematics,     */
/* Springer-Verlag (1996)                                                     */
/*                                                                            */
/* @param[out] name     Method name                                           */
/* @param[out] nStage   Number of method stages                               */
/* @param[out] invLoEst One divided by the estimation of local order          */
/* @param[out] M        Coefficients for new step solution                    */
/* @param[out] E        Coefficients for error estimation                     */
/* @param[out] A        Lower triangular coefficient matrix                   */
/* @param[out] C        Lower triangular coefficient matrix                   */
/* @param[out] alpha    Y at stage i is approx. Y(T + H*Alpha_i)              */
/* @param[out] gamma    Stage i Gamma = sum(gamma[j])                         */
/* @param[out] F        Function evaluation flags                             */
/*----------------------------------------------------------------------------*/
void InitRodas3(char ** name, int * nStage, double * invLoEst, double M[], double E[],
        double A[], double C[], double alpha[], double gamma[], char F[])
{
    /* Method name */
    *name = "Rodas3";

    /* Number of stages */
    *nStage = 4;

    /* Inverse estimation of local order: 1/3 */
    *invLoEst = 0.3333333333333333;

    /* Coefficients for new step solution */
    M[0] = 2.0;
    M[1] = 0.0;
    M[2] = 1.0;
    M[3] = 1.0;

    /* Coefficients for error estimation */
    E[0] = 0.0;
    E[1] = 0.0;
    E[2] = 0.0;
    E[3] = 1.0;

    /* Lower triangular coefficient matrix A */
    A[0] = 0.0;
    A[1] = 2.0;
    A[2] = 0.0;
    A[3] = 2.0;
    A[4] = 0.0;
    A[5] = 1.0;

    /* Lower triangular coefficient matrix C */
    C[0] = 4.0;
    C[1] = 1.0;
    C[2] = -1.0;
    C[3] = 1.0;
    C[4] = -1.0;
    C[5] = -2.66666666666667;

    /* Three function evaluations */
    F[0] = 1;
    F[1] = 0;
    F[2] = 1;
    F[3] = 1;

    /* Y_stage_i ~ Y( T + H*Alpha_i ) */
    alpha[0] = 0.0;
    alpha[1] = 0.0;
    alpha[2] = 1.0;
    alpha[3] = 1.0;

    /* Gamma_i = \sum_j  gamma_{i,j}  */
    gamma[0] = 0.5;
    gamma[1] = 1.5;
    gamma[2] = 0.0;
    gamma[3] = 0.0;

} /* END InitRodas3 */


/*----------------------------------------------------------------------------*/
/* A six-stage stiffly-stable method of order 4                               */
/*                                                                            */
/* E. Harier and G. Wanner, "Solving Ordenary Differential Equations II:      */
/* stiff and differential-algebraic problems." Computational Mathematics,     */
/* Springer-Verlag (1996)                                                     */
/*                                                                            */
/* @param[out] name     Method name                                           */
/* @param[out] nStage   Number of method stages                               */
/* @param[out] invLoEst One divided by the estimation of local order          */
/* @param[out] M        Coefficients for new step solution                    */
/* @param[out] E        Coefficients for error estimation                     */
/* @param[out] A        Lower triangular coefficient matrix                   */
/* @param[out] C        Lower triangular coefficient matrix                   */
/* @param[out] alpha    Y at stage i is approx. Y(T + H*Alpha_i)              */
/* @param[out] gamma    Stage i Gamma = sum(gamma[j])                         */
/* @param[out] F        Function evaluation flags                             */
/*----------------------------------------------------------------------------*/
void InitRodas4(char ** name, int * nStage, double * invLoEst, double M[], double E[],
        double A[], double C[], double alpha[], double gamma[], char F[])
{
    /* Method name */
    *name = "Rodas4";

    /* Number of stages */
    *nStage = 6;

    /* Inverse estimation of local order: 1/4 */
    *invLoEst = 0.25;

    /* Coefficients for new step solution */
    M[0] = 1.544000000000000;
    M[1] = 6.019134481288629;
    M[2] = 12.53708332932087;
    M[3] = -0.6878860361058950;
    M[4] = 1.0;
    M[5] = 1.0;

    /* Coefficients for error estimation */
    E[0] = 0.0;
    E[1] = 0.0;
    E[2] = 0.0;
    E[3] = 0.0;
    E[4] = 0.0;
    E[5] = 1.0;

    /* Lower triangular coefficient matrix A */
    A[0] = 1.544000000000000;
    A[1] = 0.9466785280815826;
    A[2] = 0.2557011698983284;
    A[3] = 3.314825187068521;
    A[4] = 2.896124015972201;
    A[5] = 0.9986419139977817;
    A[6] = 1.221224509226641;
    A[7] = 6.019134481288629;
    A[8] = 12.53708332932087;
    A[9] = -0.6878860361058950;
    A[10] = 1.221224509226641;
    A[11] = 6.019134481288629;
    A[12] = 12.53708332932087;
    A[13] = -0.6878860361058950;
    A[14] = 1.0;

    /* Lower triangular coefficient matrix C */
    C[0] = -5.668800000000000;
    C[1] = -2.430093356833875;
    C[2] = -0.2063599157091915;
    C[3] = -0.1073529058151375;
    C[4] = -9.594562251023355;
    C[5] = -20.47028614809616;
    C[6] = 7.496443313967647;
    C[7] = -10.24680431464352;
    C[8] = -33.99990352819905;
    C[9] = 11.70890893206160;
    C[10] = 8.083246795921522;
    C[11] = -7.981132988064893;
    C[12] = -31.52159432874371;
    C[13] = 16.31930543123136;
    C[14] = -6.058818238834054;

    /* Six function evaluations */
    F[0] = 1;
    F[1] = 1;
    F[2] = 1;
    F[3] = 1;
    F[4] = 1;
    F[5] = 1;

    /* Y_stage_i ~ Y( T + H*Alpha_i ) */
    alpha[0] = 0.000;
    alpha[1] = 0.386;
    alpha[2] = 0.210;
    alpha[3] = 0.630;
    alpha[4] = 1.000;
    alpha[5] = 1.000;

    /* Gamma_i = \sum_j  gamma_{i,j}  */
    gamma[0] = 0.2500000000000000;
    gamma[1] = -0.1043000000000000;
    gamma[2] = 0.1035000000000000;
    gamma[3] = -0.03620000000000023;
    gamma[4] = 0.0;
    gamma[5] = 0.0;

} /* END InitRodas4 */


__global__
void RosenApplyA(size_t ncells32, int istage, double * K, double * Y, double * newY)
{
    size_t idx = blockDim.x*blockIdx.x + threadIdx.x;
    if(idx < ncells32*NVAR) {
        newY[idx] = Y[idx];
        for(int j=0; j<istage; ++j) {
            newY[idx] += d_A[istage*(istage-1)/2+j] * K[ncells32*NVAR*j + idx];
        }
    }
}

__global__
void RosenApplyC(size_t ncells32, int istage, double H, double * K, double * fcn, double * stage)
{
    size_t idx = blockDim.x*blockIdx.x + threadIdx.x;
    if(idx < ncells32*NVAR) {
        stage[idx] = fcn[idx];
        for(int j=0; j<istage; ++j) {
            stage[idx] += (d_C[istage*(istage-1)/2+j] / H) * K[ncells32*NVAR*j + idx];
        }
    }
}

__global__
void RosenApplyM(size_t ncells32, int nstage, double * K, double * Y, double * newY)
{
    size_t idx = blockDim.x*blockIdx.x + threadIdx.x;
    if(idx < ncells32*NVAR) {
        newY[idx] = Y[idx];
        for(int j=0; j<nstage; ++j) {
            newY[idx] += d_M[j] * K[ncells32*NVAR*j + idx];
        }
    }
}

__global__
void RosenErr(size_t ncells32, int nstage, double * K,
        double * Y, double * newY, double * errY,
        int nTol, double * abstol, double * reltol)
{
    size_t idx = blockIdx.x*blockDim.x + threadIdx.x;

    if(idx < ncells32*NVAR) {
        double err = ZERO;
        for(int j=0; j<nstage; ++j) {
            err += d_E[j] * K[ncells32*NVAR*j + idx];
        }

        int tolidx = (nTol == 1) ? 1 : idx / ncells32;
        double Ymax = fmaxf(fabsf(Y[idx]), fabsf(newY[idx]));
        double scale = abstol[tolidx] + reltol[tolidx] * Ymax;
        errY[idx] = (err * err) / (scale * scale);
    }
}

__global__
void RosenErrNorm(size_t ncells32, double * errY, double * retval)
{
    size_t idx = blockIdx.x*blockDim.x + threadIdx.x;

    if(idx < ncells32) {
        errY += idx;
        double err = ZERO;
        for (int i=0; i<NVAR; ++i) {
            err += errY[i*ncells32];
        }
        retval[idx] = sqrt(err/(double)NVAR);
    }
}

__global__
void d_RosenStageLHS1(size_t ncells32, double diag, double * jac, double * slhs)
{
    size_t idx = blockDim.x * blockIdx.x + threadIdx.x;
    if(idx < ncells32*JAC_LU_NZ) {
        slhs[idx] = -jac[idx];
    }
}

__global__
void d_RosenStageLHS2(size_t ncells32, double diag, double * jac, double * slhs)
{
    size_t idx = blockDim.x * blockIdx.x + threadIdx.x;
    if(idx < ncells32*NVAR) {
        slhs[(idx % ncells32) + JAC_LU_DIAG[idx/ncells32]*ncells32] += diag;
    }
}

/*----------------------------------------------------------------------------*/
/* Calculates the left hand side matrix for Rosenbrock stage calculation      */
/*                                                                            */
/* @param[in]  diag     Value to add to diagonal elements                     */
/* @param[in]  jac      The Jacobian                                          */
/* @param[out] slhs     Left had side matrix for Rosenbrock stage calculation */
/*----------------------------------------------------------------------------*/
void RosenStageLHS(size_t ncells32, double diag, double * d_jac, double * d_slhs)
{
    size_t nBlocks1 = ((ncells32*JAC_LU_NZ + 255) & ~255) >> 8;
    size_t nBlocks2 = ((ncells32*NVAR + 255) & ~255) >> 8;
    size_t nThreads = 256;
    // Two kernel launches to synchronize between blocks
    d_RosenStageLHS1<<<nBlocks1, nThreads>>>(ncells32, diag, d_jac, d_slhs);
    d_RosenStageLHS2<<<nBlocks2, nThreads>>>(ncells32, diag, d_jac, d_slhs);
} /* END RosenStageLHS */



/*--------------------------------- Integrate ---------------------------------*/
/* Kppa-generated time stepping integrator                                     */
/*                                                                             */
/* @param[in]     ncells Number of grid cells                                  */
/* @param[in,out] d_var  Variable species concentrations in device memory      */
/* @param[in,out] d_fix  Fixed species concentrations in device memory         */
/* @param[in]     idx    Current grid cell index                               */
/* @param[in]     tstart Integration start time                                */
/* @param[in]     tend   Integration end time                                  */
/* @param[in]     abstol Absolute integration tolerances for variable species  */
/* @param[in]     reltol Relative integration tolerances for variable species  */
/* @param[in,out] idata  Integer integration in/out parameters                 */
/* @param[in,out] rdata  Real value integration in/out parameters              */
/*-----------------------------------------------------------------------------*/
void Integrate(size_t const ncells, double d_var[82], double d_fix[1], size_t
    const idx, double const tstart, double const tend, double const
    abstol[82], double const  reltol[82], int idata[20], double rdata[20])
{
    /* .................... Rosenbrock method parameters .................... */

    char * name;        /* Method name */
    int nStage;         /* Number of stages, from 2 to 6 */
    double invLoEst;     /* Inverse local order estimation */
    double M[6];         /* New step solution coefficients */
    double E[6];         /* Error estimation coefficients */
    double alpha[6];     /* Y_stage_i ~ Y( T + H*alpha_i ) */
    double gamma[6];     /* Gamma_i = \sum_j gamma_{i,j} */

    /* Coefficient matrices A and C are strictly lower triangular.
     * The subdiagonal elements are stored in row-wise order:
     * A(2,1)=A[0], A(3,1)=A[1], A(3,2)=A[2], etc. */
    double A[15];
    double C[15];

    /* F[i] == 0: stage i will re-use the function evaluation from stage i-1
     * F[i] != 0: stage i will evaluate function */
    char F[6];

    /* .................... Integration parameters .................... */

    double spanT;        /* Integration time span (positive value) */
    int autonomous;     /* idata[0]: Zero if F = F(t,y) */
    int nTol;           /* idata[1]: Length of the tolerance vectors, 1 = scalar*/
    int stepMax;        /* idata[2]: Maximum permitted steps */
    double minH;         /* rdata[0]: Integration step size lower bound */
    double maxH;         /* rdata[1]: Integration step size upper bound */
    double startH;       /* rdata[2]: Starting integration step size */
    double minFact;      /* rdata[3]: Lower bound on step decrease factor */
    double maxFact;      /* rdata[4]: Upper bound on step increase factor */
    double rejectFact;   /* rdata[5]: Step decrease factor after step rejection */
    double safeFact;     /* rdata[6]: Safety factor in computation of new step size */

    /* .................... Local variables .................... */

    double * d_K = 0;        /* Stage solution vectors */
    double * d_newVar = 0;   /* Variable concentrations after successful solve */
    double * d_errVar = 0;   /* Error in newVar */
    double * d_fcn0 = 0;     /* Function at time tstart */
    double * d_fcn = 0;      /* Function at time T */
    double * d_dFdT = 0;     /* Partial derivative of the function w.r.t T */
    double * d_rct = 0;      /* Reaction rates at time T */
    double * d_jac0 = 0;     /* Jacobian at time tstart */
    double * d_slhs = 0;     /* Stage computation left hand side matrix */
    double * d_abstol = 0;   /* Device memory absolute tolerance vector */
    double * d_reltol = 0;   /* Device memory relative tolerance vector */
    double * d_errNorm = 0;  /* Device memory scaled norms of error vectors */
    double * d_scratch = 0;  /* Device scratch memory */
    double * errNorm = 0;    /* Scaled norms of error vectors for all grid cells */

    int dir;            /* +1 if time advances positively, -1 otherwise */
    double T;            /* Model time */
    double H;            /* Timestep */
    double newH;         /* Updated timestep */
    double errNormMax;   /* Maximum of all scaled norms of error vectors */

    int rejectH;        /* Number of consecutive time step rejections */
    int i;              /* Iterators */

    int nFun = 0;       /* Number of function evaluations */
    int nJac = 0;       /* Number of Jacobian evaluations */
    int nStp = 0;       /* Number of solver steps */
    int nAcc = 0;       /* Number of accepted steps */
    int nRej = 0;       /* Number of rejected steps */
    int nDec = 0;       /* Number of matrix decompositions */
    int nSol = 0;       /* Number of Ax=b solves */
    int nSng = 0;       /* Number of singular decomposition results */

    /* Macro to clean up and abort the integrator */
    #define ABORT(code, fmt, ...) { \
        printf("Kppa: %s: T=%g, H=%g: " fmt, name, T, H, ##__VA_ARGS__); \
        idata[19] = code; \
        goto end; \
    }

    /* ................ Initialize the Rosenbrock method ................ */

    name = "Unknown";
    switch (idata[3]) {
    case 0:
        InitRos4(&name, &nStage, &invLoEst, M, E, A, C, alpha, gamma, F);
        break;
    case 1:
        InitRos2(&name, &nStage, &invLoEst, M, E, A, C, alpha, gamma, F);
        break;
    case 2:
        InitRos3(&name, &nStage, &invLoEst, M, E, A, C, alpha, gamma, F);
        break;
    case 3:
        InitRos4(&name, &nStage, &invLoEst, M, E, A, C, alpha, gamma, F);
        break;
    case 4:
        InitRodas3(&name, &nStage, &invLoEst, M, E, A, C, alpha, gamma, F);
        break;
    case 5:
        InitRodas4(&name, &nStage, &invLoEst, M, E, A, C, alpha, gamma, F);
        break;
    default:
        fprintf(stderr, "Kppa: Unknown method: %d\n", idata[3]);
        idata[19] = -3;
        return;
    }

    /* ................... Initialize local variables ................... */

    /* ncells rounded up to next multiple of 32 */
    size_t ncells32 = (ncells + 31) & ~31;

    /* Size in bytes of the variable concentrations in the integration */
    size_t varsize = ncells32*NVAR*sizeof(double);

    /* Number of threads in each CUDA block */
    size_t nThreads = 256;
    /* Number of blocks in CUDA grid */
    size_t nBlocks = ((ncells32*NVAR + 255) & ~255) >> 8;
    /* Number of blocks for errNorm kernel call */
    size_t errNormBlocks = ((ncells32 + 255) & ~255) >> 8;

    /* Initialize step rejection counter */
    rejectH = 0;

    /* Initialize time */
    dir = (tend >= tstart ? +1 : -1);
    spanT = dir * (tend - tstart);
    T = tstart;
    H = spanT;

    /* Determine if F depends on time */
    autonomous = (idata[0] != 0);

    /* Scalar tolerances limits the tolerance vectors to the first element. */
    nTol = idata[1] ? 1 : NVAR;

    /* Maximum number of steps before the method aborts */
    stepMax = idata[2] ? idata[2] : 100000;
    if (stepMax < 0)
        ABORT(-3, "Invalid maximum steps: %d\n", stepMax);

    /* Check tolerance vectors */
    if(idata[4]) {
        for (i=0; i<nTol; i++) {
            if (abstol[i] <= ZERO)
                ABORT(-3, "Unreasonable tolerance: abstol[%d]=%g\n", i, abstol[i]);
            if (reltol[i] <= (10.0 * DBL_EPSILON) || reltol[i] >= ONE)
                ABORT(-3, "Unreasonable tolerance: reltol[%d]=%g\n", i, reltol[i]);
        }
    }

    /* Lower bound on the step size: (positive value) */
    minH = rdata[0];
    if (minH < ZERO)
        ABORT(-3, "Invalid step size lower bound: %g\n", minH);

    /* Upper bound on the step size: (positive value) */
    maxH = rdata[1] ? fmin(fabs(rdata[1]), spanT) : spanT;
    if (maxH < ZERO)
        ABORT(-3, "Invalid step size upper bound: %g\n", maxH);

    /*  Starting step size: (positive value) */
    startH = rdata[2] ? fmin(fabs(rdata[2]), spanT) : fmax(minH,MIN_DELT);
    if (startH < ZERO)
        ABORT(-3, "Invalid starting step size: %g\n", startH);

    /* Lower bound on step decrease factor */
    minFact = rdata[3] ? rdata[3] : 0.2;
    if (minFact < ZERO)
        ABORT(-3, "Invalid lower bound on step decrease factor: %g\n", minFact);

    /* Upper bound on step increase factor */
    maxFact = rdata[4] ? rdata[4] : 6.0;
    if (maxFact < minFact)
        ABORT(-3, "Invalid upper bound on step increase factor: %g\n", maxFact);

    /* Step decrease factor after step rejection */
    rejectFact = rdata[5] ? rdata[5] : 0.1;
    if (rejectFact < ZERO)
        ABORT(-3, "Invalid step decrease factor for rejected step: %g\n", rejectFact);

    /* Safety factor in the computation of new step size */
    safeFact = rdata[6] ? rdata[6] : 0.9;
    if (safeFact < ZERO)
        ABORT(-3, "Invalid new step safety factor: %g\n", safeFact);

    /* Adjust timestep according to user-specified limits */
    H = fmin(startH, maxH);
    if (fabs(H) < 10 * DBL_EPSILON)
        H = MIN_DELT;
        
    /* Allocate memory */
    if(cudaMalloc(&d_K, nStage*varsize) != cudaSuccess)
        ABORT(-20, "Can't allocate K on device.\n");
    if(cudaMalloc(&d_newVar, varsize) != cudaSuccess)
        ABORT(-20, "Can't allocate newVar on device.\n");
    if(cudaMalloc(&d_errVar, varsize) != cudaSuccess)
        ABORT(-20, "Can't allocate errVar on device.\n");
    if(cudaMalloc(&d_fcn0, varsize) != cudaSuccess)
        ABORT(-20, "Can't allocate fcn0 on device.\n");
    if(cudaMalloc(&d_fcn, varsize) != cudaSuccess)
        ABORT(-20, "Can't allocate fcn on device.\n");
    if(cudaMalloc(&d_dFdT, varsize) != cudaSuccess)
        ABORT(-20, "Can't allocate dFdT on device.\n");
    if(cudaMalloc(&d_rct, ncells32*NREACT*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate rct on device.\n");
    if(cudaMalloc(&d_jac0, ncells32*JAC_LU_NZ*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate jac0 on device.\n");
    if(cudaMalloc(&d_slhs, ncells32*JAC_LU_NZ*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate slhs on device.\n");
    if(cudaMalloc(&d_abstol, NVAR*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate abstol on device.\n");
    if(cudaMalloc(&d_reltol, NVAR*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate reltol on device.\n");
    if(cudaMalloc(&d_errNorm, ncells32*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate errNorm on device.\n");
    if(cudaMalloc(&d_scratch, ncells32*JAC_LU_NZ*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate scratch on device.\n");
    if(cudaMallocHost(&errNorm, ncells32*sizeof(double)) != cudaSuccess)
        ABORT(-20, "Can't allocate paged locked errNorm on host.\n");

    /* Copy tolerance vectors to device */
    if(nTol > 1) {
        if(cudaMemcpy(d_abstol, abstol, NVAR*sizeof(double), cudaMemcpyHostToDevice) != cudaSuccess)
            ABORT(-20, "Can't copy absolute tolerance vector to device.\n");
        if(cudaMemcpy(d_reltol, reltol, NVAR*sizeof(double), cudaMemcpyHostToDevice) != cudaSuccess)
            ABORT(-20, "Can't copy relative tolerance vector to device.\n");
    }

    /* Copy Rosenbrock method parameters to device constant memory */
    if(cudaMemcpyToSymbol(d_M, M, sizeof(M), 0, cudaMemcpyHostToDevice) != cudaSuccess)
        ABORT(-20, "Can't copy new step solution coefficients to device.\n");
    if(cudaMemcpyToSymbol(d_E, E, sizeof(E), 0, cudaMemcpyHostToDevice) != cudaSuccess)
        ABORT(-20, "Can't copy error estimation coefficients to device.\n");
    if(cudaMemcpyToSymbol(d_A, A, sizeof(A), 0, cudaMemcpyHostToDevice) != cudaSuccess)
        ABORT(-20, "Can't copy coefficient matrix A to device.\n");
    if(cudaMemcpyToSymbol(d_C, C, sizeof(C), 0, cudaMemcpyHostToDevice) != cudaSuccess)
        ABORT(-20, "Can't copy coefficient matrix C to device.\n");

    /* ............................ Integrate ............................ */

    while(fabs(tend - T) > DBL_EPSILON) {   /* Time integration loop */

        /* Check step count */
        if (nStp > stepMax)
            ABORT(-6, "Too many integration steps: stepMax=%d\n", stepMax);

        /* Check timestep size */
        if ((T + 0.1*H == T) || (H <= DBL_EPSILON))
            ABORT(-7, "Step size too small (T + H/10 = T) or H < eps\n");

        /* Update timestep */
        H = fmin(H,fabs(tend-T));

        /* Compute reaction rates at the current time */
        Rates(ncells32, T, idx, d_rct);

        /* Compute the function at the current time */
        Fun(ncells32, d_var, d_fix, d_rct, d_fcn0, d_scratch);
        ++nFun;

        /* Compute the Jacobian at the current time */
        Jac(ncells32, d_var, d_fix, d_rct, d_jac0, d_scratch);
        ++nJac;

        /* Compute the function derivative with respect to time */
        if (!autonomous) {
            double delta = sqrt(FLT_EPSILON) * fmax(MIN_DELT, fabs(T));
            Rates(ncells32, T+delta, idx, d_rct);
            Fun(ncells32, d_var, d_fix, d_rct, d_fcn, d_scratch);
            ++nFun;
            WYMXDA(ncells32, NVAR, d_fcn0, d_fcn, delta, d_dFdT);
        }

        /* Repeat step calculation until step accepted  */
        do {
            int singRow = 0;
            int decomps = 1;

            /* Prepare the LHS matrix for stage calculations */
            RosenStageLHS(ncells32, 1.0/(dir*H*gamma[0]), d_jac0, d_slhs);

            /* LU decompose stage LHS matrix */
            singRow = Decomp(ncells32, d_slhs);
            ++nDec;

            /* If the decomposition failed, half the timestep and try again */
            while(singRow) {
                printf("Kppa: %s: LU decomposition singular on row %d\n", name, singRow-1);
                ++nSng;

                /* Reduce step size */
                H *= HALF;

                /* Abort after eight failed decompositions */
                if (decomps > 8 || H == ZERO)
                    ABORT(-8, "Matrix is repeatedly singular\n");

                /* Build new stage LHS with reduced time step */
                RosenStageLHS(ncells32, 1.0/(dir*H*gamma[0]), d_jac0, d_slhs);

                /* LU decompose stage LHS matrix */
                singRow = Decomp(ncells32, d_slhs);
                ++nDec;
                ++decomps;
            }

            /* Compute stage 0 using the previously-computed function */
            WCOPY(ncells32, NVAR, d_fcn0, d_fcn);
            WCOPY(ncells32, NVAR, d_fcn0, d_K);
            if (!autonomous && gamma[0]) {
                WAXPY(ncells32, NVAR, dir*H*gamma[0], d_dFdT, d_K);
            }

            /* Solve stage 0 */
            Solve(ncells32, d_slhs, d_K);
            nSol++;

            /* Compute the remaining stages  */
            for (int i=1; i<nStage; ++i) {
                double * d_stage = d_K + ncells32 * NVAR * i;

                if (F[i]) {
                    double tau = T + alpha[i] * dir * H;

                    /* Apply coefficient matrix A */
                    RosenApplyA<<<nBlocks, nThreads>>>(ncells32, i, d_K, d_var, d_newVar);

                    /* Update reaction rates, if necessary */
                    if(!autonomous) {
                        Rates(ncells32, tau, idx, d_rct);
                    }

                    /* Evaluate the function */
                    Fun(ncells32, d_newVar, d_fix, d_rct, d_fcn, d_scratch);
                    ++nFun;
                }

                /* Apply coefficient matrix C */
                RosenApplyC<<<nBlocks, nThreads>>>(ncells32, i, dir*H, d_K, d_fcn, d_stage);

                if (!autonomous && gamma[i]) {
                    WAXPY(ncells32, NVAR, dir*H*gamma[i], d_dFdT, d_stage);
                }

                /* Solve stage i */
                Solve(ncells32, d_slhs, d_stage);
                nSol++;
            }

            /* Compute the new solution */
            RosenApplyM<<<nBlocks, nThreads>>>(ncells32, nStage, d_K, d_var, d_newVar);

            /* Calculate error vectors */
            RosenErr<<<nBlocks, nThreads>>>(ncells32, nStage, d_K, d_var, d_newVar, d_errVar, nTol, d_abstol, d_reltol);

            /* Calculate the scaled norm of the error vector in each grid cell */
            RosenErrNorm<<<errNormBlocks, nThreads>>>(ncells32, d_errVar, d_errNorm);

            /* Find the largest scaled norm */
            cudaMemcpy(errNorm, d_errNorm, ncells32*sizeof(double), cudaMemcpyDeviceToHost);
            errNormMax = 0;
            for(int i=0; i<ncells; ++i) {
                double nrm = errNorm[i];
                if(isinf(nrm)) {
                    ABORT(-10, "Error norm in cell %zu is Inf\n", idx);
                } else if(isnan(nrm)) {
                    ABORT(-10, "Error norm in cell %zu is NaN\n", idx);
                }
                errNormMax = fmaxf(errNormMax, nrm);
            }
            rdata[12] = errNormMax;

            /* Calculate a new step size: minFact <= newH/H <= maxFact */
            newH = H * fmin(maxFact,fmax(minFact,safeFact/pow(errNormMax,invLoEst)));
            ++nStp;

            /* Decide to accept or reject step  */
            if (errNormMax <= ONE || H <= minH) {
                /* Step accepted */
                ++nAcc;
                WCOPY(ncells32, NVAR, d_newVar, d_var);
                T += dir * H;
                /* Adjust step size */
                newH = fmax(minH,fmin(newH,maxH));
                if(rejectH) {
                    newH = fmin(newH,H);
                }
                rejectH = 0;
                H = newH;
                /* Return to time loop */
                break;
            } else {
                /* Step rejected */
                ++nRej;
                if(rejectH > 1) {
                    newH = H * rejectFact;
                }
                ++rejectH;
                H = newH;
                /* Continue step calculation */
                continue;
            }

        } while(1); /* Step calculation */

    } /* Time loop */

    /* ...................... Exit integrator ...................... */
    
    /* Set exit status */
    idata[19] = 0;

end:

    /* Deallocate memory */
    cudaFree((void*)d_K);
    cudaFree((void*)d_newVar);
    cudaFree((void*)d_errVar);
    cudaFree((void*)d_fcn0);
    cudaFree((void*)d_fcn);
    cudaFree((void*)d_rct);
    cudaFree((void*)d_dFdT);
    cudaFree((void*)d_jac0);
    cudaFree((void*)d_slhs);
    cudaFree((void*)d_abstol);
    cudaFree((void*)d_reltol);
    cudaFree((void*)d_errNorm);
    cudaFree((void*)d_scratch);
    cudaFreeHost((void*)errNorm);

    /* Collect statistics */
    idata[10] = nFun;
    idata[11] = nJac;
    idata[12] = nStp;
    idata[13] = nAcc;
    idata[14] = nRej;
    idata[15] = nDec;
    idata[16] = nSol;
    idata[17] = nSng;
    /* Record exit time and last step size */
    rdata[10] = T;
    rdata[11] = H;
    rdata[12] = errNormMax;

}/* END Integrate */



/*----------------------- END kppa_CUDA_rosenbrock.h END ----------------------*/

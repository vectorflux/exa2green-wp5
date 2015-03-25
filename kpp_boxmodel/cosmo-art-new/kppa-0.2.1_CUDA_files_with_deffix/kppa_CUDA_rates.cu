/*----------------------- BEGIN kppa_CUDA_rates.cu BEGIN ----------------------*/
/* @file kppa_CUDA_rates.cu                                                    */
/* @author charlesj                                                            */
/* @date 2015-01-22 16:21:35.954951                                            */
/* @brief Reaction rate calculation and utility functions                      */
/*                                                                             */
/* Reaction rate calculation and utility functions                             */
/*                                                                             */
/* This file was generated by Kppa: http://www.paratools.com/Kppa              */
/*-----------------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include "kppa_CUDA_cu_parameters.h"
#include "kppa_CUDA_rates.h"

/* BEGIN INLINE declared at /users/charlesj/KPP_BOXMODEL/cosmo-art-new/kppa-0.2.1_CUDA_files/radm2_kpp_eleni_0714_kppa.def:308,1 */

__device__
double TROE(double const K0300, double const Q, double const KU300, double const R, double const M, double const T)
{
   double TT, K0, KU, K0M, KK, LGKK, E, F;

   TT = T / 3.e2;
   K0 = K0300 / pow(TT,Q);
   KU = KU300 / pow(TT,R);
   K0M = K0 * M;
   KK = K0M / KU;
   LGKK = 0.434294481 * log(KK);
   E = 1.0 / (1.0 + LGKK*LGKK);
   F = pow(0.6,E);
   return F * K0M / (1. + KK);
}

/*------------------------------------ EQT ------------------------------------*/
/* @param[in]     K0300  None                                                  */
/* @param[in]     Q      None                                                  */
/* @param[in]     KU300  None                                                  */
/* @param[in]     R      None                                                  */
/* @param[in]     M      None                                                  */
/* @param[in]     T      None                                                  */
/* @param[in]     A      None                                                  */
/* @param[in]     B      None                                                  */
/*-----------------------------------------------------------------------------*/
__device__
double EQT(double const K0300, double const Q, double const KU300, double const R, double const M, double const T, double const A, double const B)
{
   double KH;

   KH = TROE( K0300, Q, KU300, R, M, T );
   return (KH * A * exp( -B / T )); 
}

/*----------------------------------- SPEZ ------------------------------------*/
/* @param[in]     A0      None                                                 */
/* @param[in]     B0      None                                                 */
/* @param[in]     A2      None                                                 */
/* @param[in]     B2      None                                                 */
/* @param[in]     A3      None                                                 */
/* @param[in]     B3      None                                                 */
/* @param[in]     M       None                                                 */
/* @param[in]     T       None                                                 */
/*-----------------------------------------------------------------------------*/
__device__
double SPEZ(double const A0, double const B0, double const A2, double const B2, double const A3, double const B3, double const M, double const T)
{
   double K0, K2, K3;

   K0 = A0*exp(B0/T);
   K2 = A2*exp(B2/T);
   K3 = A3*M*exp(B3/T);
   return K0 + K3 / ( 1 + K3/K2 );
}


/* END INLINE declared at /users/charlesj/KPP_BOXMODEL/cosmo-art-new/kppa-0.2.1_CUDA_files/radm2_kpp_eleni_0714_kppa.def:308,1 */


/* Be friendly to Fortran mathmatical intrinsics */
#define SQRT(X)    sqrtf(x)
#define DSQRT(X)   sqrt(x)
#define EXP(X)     expf(x)
#define DEXP(X)    exp(x)
#define LOG(x)     log(x)
#define ALOG(X)    logf(x)
#define DLOG(X)    log(x)
#define LOG10(x)   log10(x)
#define ALOG10(X)  logf10(x)
#define DLOG10(X)  log10(x)
#define SIN(X)     sinf(x)
#define DSIN(X)    sin(x)
#define COS(X)     cosf(x)
#define DCOS(X)    cos(x)
#define TAN(X)     tanf(x)
#define DTAN(X)    tan(x)
#define POW(X,Y)   powf(x, y)
#define DPOW(X,Y)  pow(x, y)


/*------------------------------------ ARR ------------------------------------*/
/* @param[in]     a0   None                                                    */
/* @param[in]     b0   None                                                    */
/* @param[in]     c0   None                                                    */
/* @param[in]     temp Temperature                                             */
/*-----------------------------------------------------------------------------*/
__device__
double ARR(double const a0, double const b0, double const c0, double const
    temp)
{
    return __dmul_rn(__dmul_rn(a0, pow(__ddiv_rn(temp,(double)300.0), c0)),
                  exp(__ddiv_rn(-b0,temp)));
}/* END ARR */


/*------------------------------------ ARR2 -----------------------------------*/
/* @param[in]     a0   None                                                    */
/* @param[in]     b0   None                                                    */
/* @param[in]     temp Temperature                                             */
/*-----------------------------------------------------------------------------*/
__device__
double ARR2(double const a0, double const b0, double const temp)
{
    return __dmul_rn(a0, exp(__ddiv_rn(b0,temp)));
}/* END ARR2 */


/*------------------------------------ EP2 ------------------------------------*/
/* @param[in]     a0   None                                                    */
/* @param[in]     c0   None                                                    */
/* @param[in]     a2   None                                                    */
/* @param[in]     c2   None                                                    */
/* @param[in]     a3   None                                                    */
/* @param[in]     c3   None                                                    */
/* @param[in]     temp Temperature                                             */
/*-----------------------------------------------------------------------------*/
__device__
double EP2(double const a0, double const c0, double const a2, double const
    c2, double const a3, double const c3, double const temp)
{
    double k0 = __dmul_rn(a0, exp(__ddiv_rn(-c0,temp)));
    double k2 = __dmul_rn(a2, exp(__ddiv_rn(-c2,temp)));
    double k3 = __dmul_rn(a3, exp(__ddiv_rn(-c3,temp))) * (double)1.0e6*CFACTOR;
    return __dadd_rn(k0, __ddiv_rn(k3,
                __dadd_rn((double)1.0, __ddiv_rn(k3,k2))));
}/* END EP2 */


/*------------------------------------ EP3 ------------------------------------*/
/* @param[in]     a1   None                                                    */
/* @param[in]     c1   None                                                    */
/* @param[in]     a2   None                                                    */
/* @param[in]     c2   None                                                    */
/* @param[in]     temp Temperature                                             */
/*-----------------------------------------------------------------------------*/
__device__
double EP3(double const a1, double const c1, double const a2, double const
    c2, double const temp)
{
    double k1 = __dmul_rn(a1, exp(__ddiv_rn(-c1,temp)));
    double k2 = __dmul_rn(a2, exp(__ddiv_rn(-c2,temp)));
    return __dadd_rn(k1, __dmul_rn(k2, (double)1.0e6*CFACTOR));
}/* END EP3 */


/*------------------------------------ FALL -----------------------------------*/
/* @param[in]     a0   None                                                    */
/* @param[in]     b0   None                                                    */
/* @param[in]     c0   None                                                    */
/* @param[in]     a1   None                                                    */
/* @param[in]     b1   None                                                    */
/* @param[in]     c1   None                                                    */
/* @param[in]     cf   None                                                    */
/* @param[in]     temp Temperature                                             */
/*-----------------------------------------------------------------------------*/
__device__
double FALL(double const a0, double const b0, double const c0, double const
    a1, double const b1, double const c1, double const cf, double const temp)
{
    /* Accuracy trumps precision in these calculations */
    double k0 = a0 * pow(temp/(double)300.0, c0) * exp(-b0/temp) * (double)1.0e6*CFACTOR; 
    double k1 = k0 / (a1 * pow(temp/(double)300.0, c1) * exp(-b1/temp));
    return (k0/((double)1.0+k1)) * pow(cf, (double)1.0/((double)1.0+pow(log10(k1),(double)2.0)));
}/* END FALL */


/*---------------------------------- Sunlight ---------------------------------*/
/* Calculates sunlight intensity in the range [0,1] as a function of time.     */
/* Modify this routine to get the correct sunlight values for your model.      */
/*                                                                             */
/* @param[in]     time Integration time                                        */
/* @param[in]     idx  Current grid cell index                                 */
/*-----------------------------------------------------------------------------*/
__device__
double Sunlight(double const time, size_t const idx)
{
    int daysec = 24 * 3600;     /* Seconds per day */
    float sunrise = 5.5*3600;   /* 5:30 local time */
    float sunset = 19.5*3600;   /* 7:30 local time */

    float daily = time - ((int)time / daysec) * daysec;
    float tmp;

    /* Estimate sunlight intensity in the range [0,1] */
    if ((daily >= sunrise) && (daily <= sunset)) {
        tmp = __ddiv_rn(2.0*daily - sunrise-sunset, sunset-sunrise);
        tmp = (tmp > 0) ? tmp * tmp : -tmp * tmp;
        tmp = 0.5 * (1.0 + cospi(tmp));
    } else {
        tmp = 0.0;
    }
    return tmp;
}/* END Sunlight */


/*------------------------------------ TROE -----------------------------------*/
/* Troe reactions (Stockwell et. al., 1997)                                    */
/*                                                                             */
/* @param[in]     k0_300K   None                                               */
/* @param[in]     n         None                                               */
/* @param[in]     kinf_300K None                                               */
/* @param[in]     m         None                                               */
/* @param[in]     temp      Temperature                                        */
/* @param[in]     cair      None                                               */
/*-----------------------------------------------------------------------------*/
__device__
double TROE(double const k0_300K, double const n, double const kinf_300K,
    double const m, double const temp, double const cair)
{
    double zt_help = 300.0/temp;
    double k0_T = k0_300K * pow(zt_help, n) * cair;
    double kinf_T = kinf_300K * pow(zt_help, m);
    double k_ratio = k0_T/kinf_T;

    return k0_T / (1.0 + k_ratio) * pow(0.6, 1.0/(1.0+pow(log10(k_ratio), 2)));
}/* END TROE */


/*----------------------------------- TROEE -----------------------------------*/
/* Troe equilibrium reactions (Stockwell et. al., 1997)                        */
/*                                                                             */
/* @param[in]     a0        None                                               */
/* @param[in]     b0        None                                               */
/* @param[in]     k0_300K   None                                               */
/* @param[in]     n         None                                               */
/* @param[in]     kinf_300K None                                               */
/* @param[in]     m         None                                               */
/* @param[in]     temp      Temperature                                        */
/* @param[in]     cair      None                                               */
/*-----------------------------------------------------------------------------*/
__device__
double TROEE(double const a0, double const b0, double const k0_300K, double
    const n, double const kinf_300K, double const m, double const temp,
    double const cair)
{
    double zt_help = 300.0/temp;
    double k0_T = k0_300K * pow(zt_help,n) * cair;
    double kinf_T = kinf_300K * pow(zt_help,m);
    double k_ratio = k0_T/kinf_T;
    double troe = k0_T / (1.0 + k_ratio) * pow(0.6, 1.0/(1.0+pow(log10(k_ratio), 2)));

    return a0 * exp(-b0 / temp) * troe;
}/* END TROEE */


/*-------------------------------- Temperature --------------------------------*/
/* Calculates temperature (kelvin) as a function of time.                      */
/* Modify this routine to get the correct temperature values for your model.   */
/*                                                                             */
/* @param[in]     time Integration time                                        */
/* @param[in]     idx  Current grid cell index                                 */
/*-----------------------------------------------------------------------------*/
__device__
double Temperature(double const time, size_t const idx)
{
    float mintemp = 280;    /* 280 Kelvin ~= 44 Fahrenheit */
    float maxtemp = 300;    /* 300 Kelvin ~= 80 Fahrenheit */
    float tmp;

    /* Estimate temperature cycling from mintemp to maxtemp */
    tmp = sinpi(__ddiv_rn(time,24*3600));
    if (tmp < 0) {
        tmp = mintemp - tmp * (maxtemp-mintemp);
    } else {
        tmp = mintemp + tmp * (maxtemp-mintemp);
    }
    return tmp;
}/* END Temperature */



/*---------------------------------- d_Rates ----------------------------------*/
/* CUDA kernel for Rates                                                       */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in]     time     Integration time                                    */
/* @param[in]     idx      Current grid cell index                             */
/* @param[out]    rct      Reaction rates                                      */
/*-----------------------------------------------------------------------------*/
__global__
void d_Rates(size_t const ncells32, double const time, size_t const idx,
    double* rct)
{
  /* Sunlight intensity: 0 to 1 inclusive (uppercase for KPP compatibility) */
  double SUN;
  /* Temperature in kelvin  (uppercase for KPP compatibility) */
  double TEMP;

  SUN = Sunlight(time, idx);
  TEMP = Temperature(time, idx);

    size_t tidx = blockDim.x*blockIdx.x + threadIdx.x;
    if(tidx < ncells32) {
        rct += tidx;
    rct[0] = PHOTO(1);
    rct[ncells32] = PHOTO(2);
    rct[2*ncells32] = PHOTO(3);
    rct[3*ncells32] = PHOTO(4);
    rct[4*ncells32] = PHOTO(5);
    rct[5*ncells32] = PHOTO(6) +
        EQT(1.8e-31,3.2e0,4.7e-12,1.4e0,M,TEMP,4.76e+26,10900.e0);
    rct[6*ncells32] = PHOTO(7);
    rct[7*ncells32] = PHOTO(8);
    rct[8*ncells32] = PHOTO(9);
    rct[9*ncells32] = PHOTO(10);
    rct[10*ncells32] = PHOTO(11);
    rct[11*ncells32] = PHOTO(12);
    rct[12*ncells32] = PHOTO(13);
    rct[13*ncells32] = PHOTO(14);
    rct[14*ncells32] = PHOTO(15);
    rct[15*ncells32] = PHOTO(16);
    rct[16*ncells32] = PHOTO(17);
    rct[17*ncells32] = PHOTO(18);
    rct[18*ncells32] = PHOTO(19);
    rct[19*ncells32] = PHOTO(20);
    rct[20*ncells32] = PHOTO(21);
    rct[21*ncells32] = M * 6.0E-34 * pow((TEMP/300.),(-2.3));
    rct[22*ncells32] = 6.50E-12 * exp( 120. / TEMP );
    rct[23*ncells32] = 2.00E-11 * exp( 130. / TEMP );
    rct[24*ncells32] = 3.20E-11 * exp( 67. / TEMP );
    rct[25*ncells32] = 2.14e-10;
    rct[26*ncells32] = 1.4E-12 * exp( -1310. / TEMP );
    rct[27*ncells32] = 1.70E-12 * exp( -940. / TEMP );
    rct[28*ncells32] = 1.10E-14 * exp( -500. / TEMP );
    rct[29*ncells32] = 3.45E-12 * exp( 270. / TEMP );
    rct[30*ncells32] = TROE( 1.8E-31, 3.2e0, 4.7E-12, 1.4e0, M, TEMP );
    rct[31*ncells32] = 2.2E-13 * exp(620./TEMP) + 1.9E-33 * M *
        exp(980./TEMP);
    rct[32*ncells32] = 3.08e-34*exp(2820./TEMP)+2.66e-54*M*exp(3180./TEMP);
    rct[33*ncells32] = 3.30E-12 * exp( -200. / TEMP );
    rct[34*ncells32] = TROE( 7.0e-31, 2.6e0, 1.5e-11, 0.5e0, M, TEMP );
    rct[35*ncells32] = 3.30E-39 * exp( 530. / TEMP );
    rct[36*ncells32] = 1.40E-13 * exp( -2470. / TEMP );
    rct[37*ncells32] = 1.80E-11 * exp( 110. / TEMP );
    rct[38*ncells32] = 2.50E-14 * exp( -1230. / TEMP );
    rct[39*ncells32] = 2.5e-12;
    rct[40*ncells32] = TROE( 2.2e-30, 4.3e0, 1.5e-12, 0.5e0, M, TEMP );
    rct[41*ncells32] =
        EQT(2.2e-30,4.3e0,1.5e-12,0.5e0,M,TEMP,9.09e+26,11200.e0);
    rct[42*ncells32] = HET(1);
    rct[43*ncells32] = TROE( 2.6e-30, 3.2e0, 2.4e-11, 1.3e0, M, TEMP );
    rct[44*ncells32] =
        SPEZ(7.2e-15,785.e0,4.1e-16,1440.e0,1.9e-33,725.e0,M,TEMP);
    rct[45*ncells32] = 1.30E-12 * exp( 380. / TEMP );
    rct[46*ncells32] = 4.80E-11 * exp( 250. / TEMP );
    rct[47*ncells32] = TROE( 3.0e-31, 3.3e0, 1.5e-12, 0.0e0, M, TEMP );
    rct[48*ncells32] = 1.5E-13 * ( 1.0 + 2.439E-20 * M );
    rct[49*ncells32] = TEMP * TEMP * 6.95E-18 * exp( -1280. / TEMP );
    rct[50*ncells32] = TEMP * TEMP * 1.37E-17 * exp( -444. /TEMP );
    rct[51*ncells32] = 1.59E-11 * exp( -540. / TEMP );
    rct[52*ncells32] = 1.73E-11 * exp( -380. / TEMP );
    rct[53*ncells32] = 3.64E-11 * exp( -380. / TEMP );
    rct[54*ncells32] = 2.15E-12 * exp( 411. / TEMP );
    rct[55*ncells32] = 5.32E-12 * exp( 504. / TEMP );
    rct[56*ncells32] = 1.07E-11 * exp( 549. / TEMP );
    rct[57*ncells32] = 2.10E-12 * exp( 322. / TEMP );
    rct[58*ncells32] = 1.89E-11 * exp( 116. / TEMP );
    rct[59*ncells32] = 4e-11;
    rct[60*ncells32] = 9e-12;
    rct[61*ncells32] = 6.87E-12 * exp( 256. / TEMP );
    rct[62*ncells32] = 1.20E-11 * exp( -745. / TEMP );
    rct[63*ncells32] = 1.15e-11;
    rct[64*ncells32] = 1.7e-11;
    rct[65*ncells32] = 2.8e-11;
    rct[66*ncells32] = 1e-11;
    rct[67*ncells32] = 1e-11;
    rct[68*ncells32] = 1e-11;
    rct[69*ncells32] = TEMP * TEMP *6.85E-18 * exp( -444. / TEMP );
    rct[70*ncells32] = 1.55E-11 * exp( -540. / TEMP );
    rct[71*ncells32] = 2.55E-11 * exp( 409. / TEMP );
    rct[72*ncells32] = 2.6E-12 * exp ( 380. / TEMP);
    rct[73*ncells32] = 2.E16 * exp (-13500. / TEMP);
    rct[74*ncells32] = 4.7e-12;
    rct[75*ncells32] = 1.95E16 * exp(-13543. / TEMP );
    rct[76*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[77*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[78*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[79*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[80*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[81*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[82*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[83*ncells32] = 3.50E-11 * exp( -180. / TEMP );
    rct[84*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[85*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[86*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[87*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[88*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[89*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[90*ncells32] = 6.00E-13 * exp( -2058. / TEMP );
    rct[91*ncells32] = 1.40E-12 * exp( -1900. / TEMP);
    rct[92*ncells32] = 6.00E-13 * exp( -2058. / TEMP );
    rct[93*ncells32] = 1.40E-12 * exp( -1900. / TEMP);
    rct[94*ncells32] = 1.40E-12 * exp( -1900. / TEMP);
    rct[95*ncells32] = 2.2e-11;
    rct[96*ncells32] = 2.00E-12 * exp( -2923. / TEMP );
    rct[97*ncells32] = 1.00E-11 * exp( -1895. / TEMP );
    rct[98*ncells32] = 3.23E-11 * exp( -975. / TEMP );
    rct[99*ncells32] = 5.81e-13;
    rct[100*ncells32] = 1.20E-14 * exp( -2633. / TEMP );
    rct[101*ncells32] = 1.32E-14 * exp( -2105. / TEMP );
    rct[102*ncells32] = 7.29E-15 * exp( -1136. / TEMP );
    rct[103*ncells32] = 1.23E-14 * exp( -2013. / TEMP );
    rct[104*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[105*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[106*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[107*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[108*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[109*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[110*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[111*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[112*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[113*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[114*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[115*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[116*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[117*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[118*ncells32] = 1.90E-13 * exp( 220. / TEMP );
    rct[119*ncells32] = 1.40E-13 * exp( 220. / TEMP );
    rct[120*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[121*ncells32] = 3.40E-14 * exp( 220. / TEMP );
    rct[122*ncells32] = 2.90E-14 * exp( 220. / TEMP );
    rct[123*ncells32] = 1.40E-13 * exp( 220. / TEMP );
    rct[124*ncells32] = 1.40E-13 * exp( 220. / TEMP );
    rct[125*ncells32] = 1.70E-14 * exp( 220. / TEMP );
    rct[126*ncells32] = 1.70E-14 * exp( 220. / TEMP );
    rct[127*ncells32] = 9.60E-13 * exp( 220. / TEMP );
    rct[128*ncells32] = 1.70E-14 * exp( 220. / TEMP );
    rct[129*ncells32] = 1.70E-14 * exp( 220. / TEMP );
    rct[130*ncells32] = 9.60E-13 * exp( 220. / TEMP );
    rct[131*ncells32] = 3.40E-13 * exp( 220. / TEMP );
    rct[132*ncells32] = 1.00E-13 * exp( 220. / TEMP );
    rct[133*ncells32] = 8.40E-14 * exp( 220. / TEMP );
    rct[134*ncells32] = 7.20E-14 * exp( 220. / TEMP );
    rct[135*ncells32] = 3.40E-13 * exp( 220. / TEMP );
    rct[136*ncells32] = 3.40E-13 * exp( 220. / TEMP );
    rct[137*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[138*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[139*ncells32] = 1.19E-12 * exp( 220. / TEMP );
    rct[140*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[141*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[142*ncells32] = 1.19E-12 * exp( 220. / TEMP );
    rct[143*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[144*ncells32] = 1.70E-14 * exp( 220. / TEMP );
    rct[145*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[146*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[147*ncells32] = 4.20E-12 * exp( 180. / TEMP );
    rct[148*ncells32] = 7.70E-14 * exp( 1300. / TEMP );
    rct[149*ncells32] = 1.70E-14 * exp( 220. / TEMP );
    rct[150*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[151*ncells32] = 1.70E-14 * exp( 220. / TEMP );
    rct[152*ncells32] = 4.20E-14 * exp( 220. / TEMP );
    rct[153*ncells32] = 3.60E-16 * exp( 220. / TEMP );
    rct[154*ncells32] = 1.21E-11 * exp( 444. / TEMP );
    rct[155*ncells32] = ARR2(1.19E-12,490.);
    rct[156*ncells32] = ARR2(1.01E-15,-736.);
    rct[157*ncells32] = 4e-12;
    rct[158*ncells32] = 1.5e-11;
    rct[159*ncells32] = ARR2(3.56E-14,708.);
    rct[160*ncells32] = ARR2(7.40E-13,765.);
    rct[161*ncells32] = 1.2e-12;
    rct[162*ncells32] = 1.7e-10;
    rct[163*ncells32] = 1.22e-11;
    rct[164*ncells32] = 2e-16;
    rct[165*ncells32] = 4e-12;
    rct[166*ncells32] = 1.5e-11;
    rct[167*ncells32] = ARR2(3.56E-14,708.);
    rct[168*ncells32] = ARR2(7.40E-13,765.);
    rct[169*ncells32] = 1.2e-12;
    rct[170*ncells32] = ARR2(2.43E-12,360.);
    rct[171*ncells32] = ARR2(2.05E-13,1300.);
    rct[172*ncells32] = 2e-12;
    rct[173*ncells32] = 1e-10;
    rct[174*ncells32] = 1.3e-11;
    rct[175*ncells32] = 0.5*(4.13E-12 * exp( 452. / TEMP ) + 1.86E-11 * exp(
        175. / TEMP ));
    rct[176*ncells32] = 0.5*(1.36E-15 * exp( -2112. / TEMP ) + 7.51E-16 *
        exp( -1521. / TEMP ));
    rct[177*ncells32] = ARR2(2.54E-12,360.);
    rct[178*ncells32] = ARR2(1.82E-13,1300.);
    rct[179*ncells32] = 2e-12;
    rct[180*ncells32] = TROE( 9.7e-29, -5.6e0, 9.3e-12, -1.5e0, M, TEMP );
    rct[181*ncells32] = TROE( 9.7e-29, -5.6e0, 9.3e-12, -1.5e0, M, TEMP
        )/(ARR2(9.0E-19,14000.));
    rct[182*ncells32] = 3.6e-12;
    rct[183*ncells32] = 3e-11;
    rct[184*ncells32] = 3e-12;
    rct[185*ncells32] = ARR2(5.60E-12,270.);
    rct[186*ncells32] = ARR2(1.9E-13,500.);
    rct[187*ncells32] = ARR2(9.6E-12,-234.);
    rct[188*ncells32] =
        ARR2(3.04E-12,350.)*ARR2(1.106E-31,7460.)*M/(1+ARR2(1.106E-31,7460.)*M);
    rct[189*ncells32] = 5.8e-11;
    rct[190*ncells32] = 2.5e-12;
    rct[191*ncells32] = 2.5e-12;
    rct[192*ncells32] = 2.5e-12;
    rct[193*ncells32] = 0.0;
    }
}/* END d_Rates */


/*----------------------------------- Rates -----------------------------------*/
/* Calculates reaction rate coefficients                                       */
/*                                                                             */
/* @param[in]     ncells32 A multiple of 32 grid cells                         */
/* @param[in]     time     Integration time                                    */
/* @param[in]     idx      Current grid cell index                             */
/* @param[out]    d_rct    Reaction rates in device memory                     */
/*-----------------------------------------------------------------------------*/
void Rates(size_t const ncells32, double const time, size_t const idx,
    double* d_rct)
{
    size_t nBlocks = ((ncells32 + 127) & ~127) >> 7;
    size_t nThreads = 128;
    d_Rates<<<nBlocks, nThreads>>>(ncells32, time, idx, d_rct);
}/* END Rates */


/*------------------------- END kppa_CUDA_rates.h END -------------------------*/

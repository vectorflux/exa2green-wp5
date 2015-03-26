! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! The Reaction Rates File
! 
! Generated by KPP-2.2 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : box_model_Rates.f90
! Time                 : Tue Jun  3 15:01:53 2014
! Working directory    : /Users/jlinford/workspace/kppa/verify/box_model_kpp221
! Equation file        : box_model.kpp
! Output root filename : box_model
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE box_model_Rates

  USE box_model_Parameters
  USE box_model_Global
  IMPLICIT NONE

CONTAINS



! Begin Rate Law Functions from KPP_HOME/util/UserRateLaws

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  User-defined Rate Law functions
!  Note: the default argument type for rate laws, as read from the equations file, is single precision
!        but all the internal calculations are performed in double precision
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~>  Arrhenius
   REAL(kind=dp) FUNCTION ARR( A0,B0,C0 )
      REAL A0,B0,C0      
      ARR =  DBLE(A0) * EXP(-DBLE(B0)/TEMP) * (TEMP/300.0_dp)**DBLE(C0)
   END FUNCTION ARR        

!~~~> Simplified Arrhenius, with two arguments
!~~~> Note: The argument B0 has a changed sign when compared to ARR
   REAL(kind=dp) FUNCTION ARR2( A0,B0 )
      REAL A0,B0           
      ARR2 =  DBLE(A0) * EXP( DBLE(B0)/TEMP )              
   END FUNCTION ARR2          

   REAL(kind=dp) FUNCTION EP2(A0,C0,A2,C2,A3,C3)
      REAL A0,C0,A2,C2,A3,C3
      REAL(kind=dp) K0,K2,K3            
      K0 = DBLE(A0) * EXP(-DBLE(C0)/TEMP)
      K2 = DBLE(A2) * EXP(-DBLE(C2)/TEMP)
      K3 = DBLE(A3) * EXP(-DBLE(C3)/TEMP)
      K3 = K3*CFACTOR*1.0E6_dp
      EP2 = K0 + K3/(1.0_dp+K3/K2 )
   END FUNCTION EP2

   REAL(kind=dp) FUNCTION EP3(A1,C1,A2,C2) 
      REAL A1, C1, A2, C2
      REAL(kind=dp) K1, K2      
      K1 = DBLE(A1) * EXP(-DBLE(C1)/TEMP)
      K2 = DBLE(A2) * EXP(-DBLE(C2)/TEMP)
      EP3 = K1 + K2*(1.0E6_dp*CFACTOR)
   END FUNCTION EP3 

   REAL(kind=dp) FUNCTION FALL ( A0,B0,C0,A1,B1,C1,CF)
      REAL A0,B0,C0,A1,B1,C1,CF
      REAL(kind=dp) K0, K1     
      K0 = DBLE(A0) * EXP(-DBLE(B0)/TEMP)* (TEMP/300.0_dp)**DBLE(C0)
      K1 = DBLE(A1) * EXP(-DBLE(B1)/TEMP)* (TEMP/300.0_dp)**DBLE(C1)
      K0 = K0*CFACTOR*1.0E6_dp
      K1 = K0/K1
      FALL = (K0/(1.0_dp+K1))*   &
           DBLE(CF)**(1.0_dp/(1.0_dp+(LOG10(K1))**2))
   END FUNCTION FALL

  !---------------------------------------------------------------------------

  ELEMENTAL REAL(kind=dp) FUNCTION k_3rd(temp,cair,k0_300K,n,kinf_300K,m,fc)

    INTRINSIC LOG10

    REAL(kind=dp), INTENT(IN) :: temp      ! temperature [K]
    REAL(kind=dp), INTENT(IN) :: cair      ! air concentration [molecules/cm3]
    REAL, INTENT(IN) :: k0_300K   ! low pressure limit at 300 K
    REAL, INTENT(IN) :: n         ! exponent for low pressure limit
    REAL, INTENT(IN) :: kinf_300K ! high pressure limit at 300 K
    REAL, INTENT(IN) :: m         ! exponent for high pressure limit
    REAL, INTENT(IN) :: fc        ! broadening factor (usually fc=0.6)
    REAL(kind=dp) :: zt_help, k0_T, kinf_T, k_ratio

    zt_help = 300._dp/temp
    k0_T    = k0_300K   * zt_help**(n) * cair ! k_0   at current T
    kinf_T  = kinf_300K * zt_help**(m)        ! k_inf at current T
    k_ratio = k0_T/kinf_T
    k_3rd   = k0_T/(1._dp+k_ratio)*fc**(1._dp/(1._dp+LOG10(k_ratio)**2))

  END FUNCTION k_3rd

  !---------------------------------------------------------------------------

  ELEMENTAL REAL(kind=dp) FUNCTION k_arr (k_298,tdep,temp)
    ! Arrhenius function

    REAL,     INTENT(IN) :: k_298 ! k at T = 298.15K
    REAL,     INTENT(IN) :: tdep  ! temperature dependence
    REAL(kind=dp), INTENT(IN) :: temp  ! temperature

    INTRINSIC EXP

    k_arr = k_298 * EXP(tdep*(1._dp/temp-3.3540E-3_dp)) ! 1/298.15=3.3540e-3

  END FUNCTION k_arr

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  End of User-defined Rate Law functions
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! End Rate Law Functions from KPP_HOME/util/UserRateLaws


! Begin INLINED Rate Law Functions



FUNCTION PHUX(X,Y,Z)
!C                                                                          
!C     CALCULATION OF PHOTOLYSIS FREQUENCY WITH ALGORITHM FROM              
!C     ROETHS FLUX-PROGAM                                                  
!C     CHI IN RADIAN                                                        
!C     X,Y,Z FROM ROETH                                                     
!C     X  IS PHOTOLYSIS FREQUENCY IN 1/S FOR SOLAR ZENITH ANGLE CHI=0 DEG   
!C                                                                          
!C     KUHN 07.09.93                                                
!C                                                                          
      IMPLICIT NONE                                                        
      REAL(8) :: X,Y,Z,CHIZ,YCHIZ,EYCHIZ, PHUX                
      REAL(8), PARAMETER :: MINYZ = -30, &
                             EMINYZ = 9.357623D-14  !EMINYZ=EXP(MINYZ) 
      REAL(8), PARAMETER :: CHI = 0.0                 
  
      CHIZ   = CHI * Z                                                     
      IF (CHIZ.LT.1.57079632679489D0) THEN                                 
         YCHIZ = Y * (1.0 - (1.0/ COS(CHIZ) ) )                            
         IF (YCHIZ.GT.MINYZ) THEN                                          
            EYCHIZ =  DEXP (YCHIZ)                                         
         ELSE                                                              
            EYCHIZ =  EMINYZ                                               
         ENDIF                                                             
      ELSE                                                                 
         EYCHIZ = EMINYZ                                                   
      ENDIF                                                                
      PHUX = X * EYCHIZ                                                    
!      PHUX =0.                 !ik: ohne photolyse
END FUNCTION PHUX


!C
!C*********************************************************************
!C Die Funktion TROE2 bestimmt die Werte der Troefunktion
!C nach Stockwell et al. [2],
!C die die Druckabhaengigkeit von Reaktionskonstanten durch eine
!C quasi-bimolekulare Reaktionskonstante beschreibt.
!C*********************************************************************
!C
 FUNCTION TROE2( K0300, Q, KU300, R, T )
!C CALCULATION OF RATE CONSTANTS FOR TROE2 REACTIONS
!      IMPLICIT REAL(8) (A-Z)
      REAL(8) :: K0300, Q, KU300, R, T, TROE2
      REAL(8) :: tt, k0, ku, k0m, kk, lgkk, e, f
      REAL(8), PARAMETER :: M = 2.55e19

      TT = T / 3.D2
      K0 = K0300 / TT**Q
      KU = KU300 / TT**R
      K0M = K0 * M
      KK = K0M / KU
      LGKK = 0.434294481D0 * LOG(KK)
      E = 1.D0 / ( 1.D0 + LGKK*LGKK )
      F = 0.6D0 ** E
      TROE2 = F * K0M / ( 1.D0 + KK )
END FUNCTION TROE2


!C
!C**********************************************************************
!C Die Funktion EQT [2] bestimmt die Reaktionskonstanten fuer
!C HNO4 --> H2O + NO2 und N2O5 --> NO2 + NO3
!C**********************************************************************
!C
FUNCTION EQT( K0300, Q, KU300, R, T, A, B )
      !IMPLICIT REAL(8) (A-Z)
      REAL(8) :: K0300, Q, KU300, R, T, A, B, EQT
      REAL(8) :: kh
      KH = TROE2( K0300, Q, KU300, R, T )
      EQT = KH * A *DEXP( -B / T )
END FUNCTION EQT

FUNCTION EQT2(T)
      !IMPLICIT REAL(8) (A-Z)
      REAL(8) :: K0300, Q, KU300, R, T, A, B, EQT2
      REAL(8) :: kh
      K0300 = 1.8d-31
      Q = 3.2d0
      KU300 = 4.7d-12
      R = 1.4d0
      A = 4.76d+26
      B = 10900.d0
      KH = TROE2( K0300, Q, KU300, R, T )
      EQT2 = KH * A *DEXP( -B / T )
END FUNCTION EQT2

!C
!C**********************************************************************
!C Die Funktion SPEZ bestimmt die Reaktionskonstante fuer
!C HNO3 + HO --> H2O + NO3
!C**********************************************************************
!C
FUNCTION SPEZ(A0,B0,A2,B2,A3,B3,T)
!C SPECIAL RATE CONSTANTS
      !IMPLICIT REAL(8) (A-Z)
      REAL(8) :: A0,B0,A2,B2,A3,B3,T, SPEZ
      REAL(8) :: k0, k2, k3
      REAL(8), PARAMETER :: M = 2.55e19
      K0 = A0*DEXP(B0/T)
      K2 = A2*DEXP(B2/T)
      K3 = A3*M*DEXP(B3/T)
      SPEZ = K0 + K3 / ( 1 + K3/K2 )
END FUNCTION SPEZ


! End INLINED Rate Law Functions

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Update_SUN - update SUN light using TIME
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE Update_SUN()
      !USE box_model_Parameters
      !USE box_model_Global

    IMPLICIT NONE

    REAL(kind=dp) :: SunRise, SunSet
    REAL(kind=dp) :: Thour, Tlocal, Ttmp 
    ! PI - Value of pi
    REAL(kind=dp), PARAMETER :: PI = 3.14159265358979d0
    
    SunRise = 4.5_dp 
    SunSet  = 19.5_dp 
    Thour = TIME/3600.0_dp 
    Tlocal = Thour - (INT(Thour)/24)*24

    IF ((Tlocal>=SunRise).AND.(Tlocal<=SunSet)) THEN
       Ttmp = (2.0*Tlocal-SunRise-SunSet)/(SunSet-SunRise)
       IF (Ttmp.GT.0) THEN
          Ttmp =  Ttmp*Ttmp
       ELSE
          Ttmp = -Ttmp*Ttmp
       END IF
       SUN = ( 1.0_dp + COS(PI*Ttmp) )/2.0_dp 
    ELSE
       SUN = 0.0_dp 
    END IF

 END SUBROUTINE Update_SUN

! End of Update_SUN function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Update_RCONST - function to update rate constants
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Update_RCONST ( )




! Begin INLINED RCONST


! End INLINED RCONST

  RCONST(1) = (PHUX(1.07E-02,1.01319E+00,0.83330E+00))
  RCONST(2) = (PHUX(3.22E-05,4.45037E+00,0.78028E+00))
  RCONST(3) = (PHUX(5.36E-04,0.34764E+00,0.91030E+00))
  RCONST(4) = (PHUX(8.96E-04,0.99438E+00,0.83295E+00))
  RCONST(5) = (PHUX(5.48E-07,2.86922E+00,0.79561E+00))
  RCONST(6) = (PHUX(3.90E-06,2.37354E+00,0.79830E+00)+EQT2(TEMP))
  RCONST(7) = (PHUX(2.74E-02,0.26226E+00,0.92849E+00))
  RCONST(8) = (PHUX(2.73E-01,0.29327E+00,0.92401E+00))
  RCONST(9) = (PHUX(7.78E-06,1.91463E+00,0.79810E+00))
  RCONST(10) = (PHUX(4.92E-05,1.60973E+00,0.80184E+00))
  RCONST(11) = (PHUX(4.05E-05,2.06917E+00,0.80267E+00))
  RCONST(12) = (PHUX(5.40E-06,2.52915E+00,0.79722E+00))
  RCONST(13) = (PHUX(6.37E-06,1.76570E+00,0.80004E+00))
  RCONST(14) = (PHUX(6.37E-06,1.76570E+00,0.80004E+00))
  RCONST(15) = (PHUX(6.10E-09,9.17009E+00,0.72585E+00))
  RCONST(16) = (PHUX(1.32E-05,2.46350E+00,0.79768E+00))
  RCONST(17) = (PHUX(3.11E-03,0.55016E+00,0.88313E+00))
  RCONST(18) = (PHUX(3.11E-03,0.55016E+00,0.88313E+00))
  RCONST(19) = (PHUX(1.85E-03,0.57967E+00,0.87921E+00))
  RCONST(20) = (PHUX(6.39E-04,1.53712E+00,0.80233E+00))
  RCONST(21) = (PHUX(7.20E-08,9.11436E+00,0.72600E+00))
  RCONST(22) = (2.55e19*6.0E-34*(TEMP/300.)**(-2.3))
  RCONST(23) = (6.50E-12*exp(120./TEMP))
  RCONST(24) = (2.00E-11*exp(130./TEMP))
  RCONST(25) = (3.20E-11*exp(67./TEMP))
! RCONST(26) = constant rate coefficient
  RCONST(27) = (1.4E-12*exp(-1310./TEMP))
  RCONST(28) = (1.70E-12*exp(-940./TEMP))
  RCONST(29) = (1.10E-14*exp(-500./TEMP))
  RCONST(30) = (3.45E-12*exp(270./TEMP))
  RCONST(31) = (TROE2(1.8E-31,3.2e0,4.7E-12,1.4e0,TEMP))
  RCONST(32) = (2.2E-13*exp(620./TEMP)+1.9E-33*2.55e19*exp(980./TEMP))
  RCONST(33) = (3.08e-34*exp(2820./TEMP)+2.66e-54*2.55e19*exp(3180./TEMP))
  RCONST(34) = (3.30E-12*exp(-200./TEMP))
  RCONST(35) = (TROE2(7.0e-31,2.6e0,1.5e-11,0.5e0,TEMP))
  RCONST(36) = (3.30E-39*exp(530./TEMP))
  RCONST(37) = (1.40E-13*exp(-2470./TEMP))
  RCONST(38) = (1.80E-11*exp(110./TEMP))
  RCONST(39) = (2.50E-14*exp(-1230./TEMP))
! RCONST(40) = constant rate coefficient
  RCONST(41) = (TROE2(2.2e-30,4.3e0,1.5e-12,0.5e0,TEMP))
  RCONST(42) = (EQT(2.2e-30,4.3e0,1.5e-12,0.5e0,TEMP,9.09e+26,11200.e0))
! RCONST(43) = constant rate coefficient
  RCONST(44) = (TROE2(2.6e-30,3.2e0,2.4e-11,1.3e0,TEMP))
  RCONST(45) = (SPEZ(7.2e-15,785.e0,4.1e-16,1440.e0,1.9e-33,725.e0,TEMP))
  RCONST(46) = (1.30E-12*exp(380./TEMP))
  RCONST(47) = (4.80E-11*exp(250./TEMP))
  RCONST(48) = (TROE2(3.0e-31,3.3e0,1.5e-12,0.0e0,TEMP))
  RCONST(49) = (1.5E-13*(1.0+2.439E-20*2.55e19))
  RCONST(50) = (TEMP*TEMP*6.95E-18*exp(-1280./TEMP))
  RCONST(51) = (TEMP*TEMP*1.37E-17*exp(-444./TEMP))
  RCONST(52) = (1.59E-11*exp(-540./TEMP))
  RCONST(53) = (1.73E-11*exp(-380./TEMP))
  RCONST(54) = (3.64E-11*exp(-380./TEMP))
  RCONST(55) = (2.15E-12*exp(411./TEMP))
  RCONST(56) = (5.32E-12*exp(504./TEMP))
  RCONST(57) = (1.07E-11*exp(549./TEMP))
  RCONST(58) = (2.10E-12*exp(322./TEMP))
  RCONST(59) = (1.89E-11*exp(116./TEMP))
! RCONST(60) = constant rate coefficient
! RCONST(61) = constant rate coefficient
  RCONST(62) = (6.87E-12*exp(256./TEMP))
  RCONST(63) = (1.20E-11*exp(-745./TEMP))
! RCONST(64) = constant rate coefficient
! RCONST(65) = constant rate coefficient
! RCONST(66) = constant rate coefficient
! RCONST(67) = constant rate coefficient
! RCONST(68) = constant rate coefficient
! RCONST(69) = constant rate coefficient
  RCONST(70) = (TEMP*TEMP*6.85E-18*exp(-444./TEMP))
  RCONST(71) = (1.55E-11*exp(-540./TEMP))
  RCONST(72) = (2.55E-11*exp(409./TEMP))
  RCONST(73) = (2.6E-12*exp(380./TEMP))
  RCONST(74) = (2.E16*exp(-13500./TEMP))
! RCONST(75) = constant rate coefficient
  RCONST(76) = (1.95E16*exp(-13543./TEMP))
  RCONST(77) = (4.20E-12*exp(180./TEMP))
  RCONST(78) = (4.20E-12*exp(180./TEMP))
  RCONST(79) = (4.20E-12*exp(180./TEMP))
  RCONST(80) = (4.20E-12*exp(180./TEMP))
  RCONST(81) = (4.20E-12*exp(180./TEMP))
  RCONST(82) = (4.20E-12*exp(180./TEMP))
  RCONST(83) = (4.20E-12*exp(180./TEMP))
  RCONST(84) = (3.50E-11*exp(-180./TEMP))
  RCONST(85) = (4.20E-12*exp(180./TEMP))
  RCONST(86) = (4.20E-12*exp(180./TEMP))
  RCONST(87) = (4.20E-12*exp(180./TEMP))
  RCONST(88) = (4.20E-12*exp(180./TEMP))
  RCONST(89) = (4.20E-12*exp(180./TEMP))
  RCONST(90) = (4.20E-12*exp(180./TEMP))
  RCONST(91) = (6.00E-13*exp(-2058./TEMP))
  RCONST(92) = (1.40E-12*exp(-1900./TEMP))
  RCONST(93) = (6.00E-13*exp(-2058./TEMP))
  RCONST(94) = (1.40E-12*exp(-1900./TEMP))
  RCONST(95) = (1.40E-12*exp(-1900./TEMP))
! RCONST(96) = constant rate coefficient
  RCONST(97) = (2.00E-12*exp(-2923./TEMP))
  RCONST(98) = (1.00E-11*exp(-1895./TEMP))
  RCONST(99) = (3.23E-11*exp(-975./TEMP))
! RCONST(100) = constant rate coefficient
  RCONST(101) = (1.20E-14*exp(-2633./TEMP))
  RCONST(102) = (1.32E-14*exp(-2105./TEMP))
  RCONST(103) = (7.29E-15*exp(-1136./TEMP))
  RCONST(104) = (1.23E-14*exp(-2013./TEMP))
  RCONST(105) = (7.70E-14*exp(1300./TEMP))
  RCONST(106) = (7.70E-14*exp(1300./TEMP))
  RCONST(107) = (7.70E-14*exp(1300./TEMP))
  RCONST(108) = (7.70E-14*exp(1300./TEMP))
  RCONST(109) = (7.70E-14*exp(1300./TEMP))
  RCONST(110) = (7.70E-14*exp(1300./TEMP))
  RCONST(111) = (7.70E-14*exp(1300./TEMP))
  RCONST(112) = (7.70E-14*exp(1300./TEMP))
  RCONST(113) = (7.70E-14*exp(1300./TEMP))
  RCONST(114) = (7.70E-14*exp(1300./TEMP))
  RCONST(115) = (7.70E-14*exp(1300./TEMP))
  RCONST(116) = (7.70E-14*exp(1300./TEMP))
  RCONST(117) = (7.70E-14*exp(1300./TEMP))
  RCONST(118) = (7.70E-14*exp(1300./TEMP))
  RCONST(119) = (1.90E-13*exp(220./TEMP))
  RCONST(120) = (1.40E-13*exp(220./TEMP))
  RCONST(121) = (4.20E-14*exp(220./TEMP))
  RCONST(122) = (3.40E-14*exp(220./TEMP))
  RCONST(123) = (2.90E-14*exp(220./TEMP))
  RCONST(124) = (1.40E-13*exp(220./TEMP))
  RCONST(125) = (1.40E-13*exp(220./TEMP))
  RCONST(126) = (1.70E-14*exp(220./TEMP))
  RCONST(127) = (1.70E-14*exp(220./TEMP))
  RCONST(128) = (9.60E-13*exp(220./TEMP))
  RCONST(129) = (1.70E-14*exp(220./TEMP))
  RCONST(130) = (1.70E-14*exp(220./TEMP))
  RCONST(131) = (9.60E-13*exp(220./TEMP))
  RCONST(132) = (3.40E-13*exp(220./TEMP))
  RCONST(133) = (1.00E-13*exp(220./TEMP))
  RCONST(134) = (8.40E-14*exp(220./TEMP))
  RCONST(135) = (7.20E-14*exp(220./TEMP))
  RCONST(136) = (3.40E-13*exp(220./TEMP))
  RCONST(137) = (3.40E-13*exp(220./TEMP))
  RCONST(138) = (4.20E-14*exp(220./TEMP))
  RCONST(139) = (4.20E-14*exp(220./TEMP))
  RCONST(140) = (1.19E-12*exp(220./TEMP))
  RCONST(141) = (4.20E-14*exp(220./TEMP))
  RCONST(142) = (4.20E-14*exp(220./TEMP))
  RCONST(143) = (1.19E-12*exp(220./TEMP))
  RCONST(144) = (7.70E-14*exp(1300./TEMP))
  RCONST(145) = (1.70E-14*exp(220./TEMP))
  RCONST(146) = (4.20E-14*exp(220./TEMP))
  RCONST(147) = (4.20E-12*exp(180./TEMP))
  RCONST(148) = (4.20E-12*exp(180./TEMP))
  RCONST(149) = (7.70E-14*exp(1300./TEMP))
  RCONST(150) = (1.70E-14*exp(220./TEMP))
  RCONST(151) = (4.20E-14*exp(220./TEMP))
  RCONST(152) = (1.70E-14*exp(220./TEMP))
  RCONST(153) = (4.20E-14*exp(220./TEMP))
  RCONST(154) = (3.60E-16*exp(220./TEMP))
  RCONST(155) = (1.21E-11*exp(444./TEMP))
  RCONST(156) = (ARR2(1.19E-12,490.))
  RCONST(157) = (ARR2(1.01E-15,-736.))
! RCONST(158) = constant rate coefficient
! RCONST(159) = constant rate coefficient
  RCONST(160) = (ARR2(3.56E-14,708.))
  RCONST(161) = (ARR2(7.40E-13,765.))
! RCONST(162) = constant rate coefficient
! RCONST(163) = constant rate coefficient
! RCONST(164) = constant rate coefficient
! RCONST(165) = constant rate coefficient
! RCONST(166) = constant rate coefficient
! RCONST(167) = constant rate coefficient
  RCONST(168) = (ARR2(3.56E-14,708.))
  RCONST(169) = (ARR2(7.40E-13,765.))
! RCONST(170) = constant rate coefficient
  RCONST(171) = (ARR2(2.43E-12,360.))
  RCONST(172) = (ARR2(2.05E-13,1300.))
! RCONST(173) = constant rate coefficient
! RCONST(174) = constant rate coefficient
! RCONST(175) = constant rate coefficient
  RCONST(176) = (0.5*(4.13E-12*exp(452./TEMP)+1.86E-11*exp(175./TEMP)))
  RCONST(177) = (0.5*(1.36E-15*exp(-2112./TEMP)+7.51E-16*exp(-1521./TEMP)))
  RCONST(178) = (ARR2(2.54E-12,360.))
  RCONST(179) = (ARR2(1.82E-13,1300.))
! RCONST(180) = constant rate coefficient
  RCONST(181) = (TROE2(9.7e-29,-5.6e0,9.3e-12,-1.5e0,TEMP))
  RCONST(182) = (TROE2(9.7e-29,-5.6e0,9.3e-12,-1.5e0,TEMP)/(ARR2(9.0E-19,14000.)))
! RCONST(183) = constant rate coefficient
! RCONST(184) = constant rate coefficient
! RCONST(185) = constant rate coefficient
  RCONST(186) = (ARR2(5.60E-12,270.))
  RCONST(187) = (ARR2(1.9E-13,500.))
  RCONST(188) = (ARR2(9.6E-12,-234.))
! RCONST(189) = constant rate coefficient
! RCONST(190) = constant rate coefficient
! RCONST(191) = constant rate coefficient
! RCONST(192) = constant rate coefficient
      
END SUBROUTINE Update_RCONST

! End of Update_RCONST function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Update_PHOTO - function to update photolytical rate constants
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Update_PHOTO ( )


   USE box_model_Global

      
END SUBROUTINE Update_PHOTO

! End of Update_PHOTO function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE box_model_Rates

!---------------------- BEGIN box_model_rates.f90 BEGIN ----------------------
! @file box_model_rates.f90                                                   
! @author charlesj                                                            
! @date 2014-10-30 21:36:02.929285                                            
! @brief Reaction rate calculation and utility functions                      
!                                                                             
! Reaction rate calculation and utility functions                             
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE box_model_rates

  USE box_model_parameters

  IMPLICIT NONE


! BEGIN INLINE declared at /users/charlesj/KPP_BOXMODEL/0-dim_box_model_Exa2Green/kppa-0.2.1_SERIAL_TEMP/box_model.def:323,1 



  CONTAINS

FUNCTION CHIBE(ZEIT,GB,GL,TAG)
      REAL(8) :: CHIBE,ZEIT,GB,GL,TAG,SD,CD,FP,OM,SH,SH1,CHI,DEKL
!C     BERECHNUNG DES SONNENWINKELS CHI
!C
      DEKL = 23.45E0*SIN((280.1E0+0.981E0*TAG)*0.01745329E0)
      FP = 0.01745329E0
      SD = SIN(DEKL*FP)
      CD = COS(DEKL*FP)
!CU    OM = (12.E0-(ZEIT+GL/15.E0))*0.261799388E0
!CU **** TEILER /60. EINGEFUEGT DA ZEIT IN MINUTEN UEBERGEBEN WIRD ****
      OM = (12.E0-(ZEIT/60.+GL/15.E0))*0.261799388E0
      SH = SIN(GB*FP)*SD+COS(GB*FP)*CD*COS(OM)
      SH1 = ASIN(SH)*180./3.141592
!CU    IF(SH1.LT.0.) SH1=0.
      CHI=90.-SH1
!cn *** fester sonnenzenitwinkel vorgeben
!cN      chi=25.
!cn *** !!!
      CHI=3.141592/180.*CHI
      CHIBE=CHI
END FUNCTION CHIBE

FUNCTION PHUX(X,Y,Z,CHI)
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
      REAL(8) :: CHI
  
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
!      PHUX =0.			!ik: ohne photolyse
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


! END INLINE declared at /users/charlesj/KPP_BOXMODEL/0-dim_box_model_Exa2Green/kppa-0.2.1_SERIAL_TEMP/box_model.def:323,1 


!------------------------------------ ARR ------------------------------------
! @param[in]     a0   None                                                    
! @param[in]     b0   None                                                    
! @param[in]     c0   None                                                    
! @param[in]     temp Temperature                                             
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION ARR(a0, b0, c0, temp)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: a0
    REAL(8), INTENT(IN) :: b0
    REAL(8), INTENT(IN) :: c0
    REAL(8), INTENT(IN) :: temp

        ARR = a0 * (temp/300.0)**c0 * EXP(-b0/temp)
  END FUNCTION ARR


!------------------------------------ ARR2 -----------------------------------
! @param[in]     a0   None                                                    
! @param[in]     b0   None                                                    
! @param[in]     temp Temperature                                             
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION ARR2(a0, b0, temp)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: a0
    REAL(8), INTENT(IN) :: b0
    REAL(8), INTENT(IN) :: temp

        ARR2 = a0 * EXP(b0/temp)
  END FUNCTION ARR2


!------------------------------------ EP2 ------------------------------------
! @param[in]     a0   None                                                    
! @param[in]     c0   None                                                    
! @param[in]     a2   None                                                    
! @param[in]     c2   None                                                    
! @param[in]     a3   None                                                    
! @param[in]     c3   None                                                    
! @param[in]     temp Temperature                                             
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION EP2(a0, c0, a2, c2, a3, c3, temp)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: a0
    REAL(8), INTENT(IN) :: c0
    REAL(8), INTENT(IN) :: a2
    REAL(8), INTENT(IN) :: c2
    REAL(8), INTENT(IN) :: a3
    REAL(8), INTENT(IN) :: c3
    REAL(8), INTENT(IN) :: temp

        REAL(8) :: k0, k2, k3

        k0 = a0 * EXP(-c0/temp)
        k2 = a2 * EXP(-c2/temp)
        k3 = a3 * EXP(-c3/temp) * 1.0e6*CFACTOR
        
        EP2 = k0 + k3/(1.0 + k3/k2)
  END FUNCTION EP2


!------------------------------------ EP3 ------------------------------------
! @param[in]     a1   None                                                    
! @param[in]     c1   None                                                    
! @param[in]     a2   None                                                    
! @param[in]     c2   None                                                    
! @param[in]     temp Temperature                                             
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION EP3(a1, c1, a2, c2, temp)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: a1
    REAL(8), INTENT(IN) :: c1
    REAL(8), INTENT(IN) :: a2
    REAL(8), INTENT(IN) :: c2
    REAL(8), INTENT(IN) :: temp

        REAL(8) :: k1, k2
        
        k1 = a1 * EXP(-c1/temp)
        k2 = a2 * EXP(-c2/temp) * 1.0e6*CFACTOR
        
        EP3 = k1 + k2
  END FUNCTION EP3


!------------------------------------ FALL -----------------------------------
! @param[in]     a0   None                                                    
! @param[in]     b0   None                                                    
! @param[in]     c0   None                                                    
! @param[in]     a1   None                                                    
! @param[in]     b1   None                                                    
! @param[in]     c1   None                                                    
! @param[in]     cf   None                                                    
! @param[in]     temp Temperature                                             
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION FALL(a0, b0, c0, a1, b1, c1, cf, temp)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: a0
    REAL(8), INTENT(IN) :: b0
    REAL(8), INTENT(IN) :: c0
    REAL(8), INTENT(IN) :: a1
    REAL(8), INTENT(IN) :: b1
    REAL(8), INTENT(IN) :: c1
    REAL(8), INTENT(IN) :: cf
    REAL(8), INTENT(IN) :: temp

        REAL(8) :: k0, k1
    
        k0 = a0 * (temp/300.0)**c0 * EXP(-b0/temp) * 1.0e6*CFACTOR
        k1 = k0 / (a1 * (temp/300.0)**c1 * EXP(-b1/temp))
        
        FALL = (k0/(1.0+k1)) * cf**(1.0/(1.0+LOG10(k1)**2.0))
  END FUNCTION FALL


!------------------------------------ TROE -----------------------------------
! Troe reactions (Stockwell et. al., 1997)                                    
!                                                                             
! @param[in]     k0_300K   None                                               
! @param[in]     n         None                                               
! @param[in]     kinf_300K None                                               
! @param[in]     m         None                                               
! @param[in]     temp      Temperature                                        
! @param[in]     cair      None                                               
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION TROE(k0_300K, n, kinf_300K, m, temp, cair)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: k0_300K
    REAL(8), INTENT(IN) :: n
    REAL(8), INTENT(IN) :: kinf_300K
    REAL(8), INTENT(IN) :: m
    REAL(8), INTENT(IN) :: temp
    REAL(8), INTENT(IN) :: cair

        REAL(8) :: zt_help, k0_T, kinf_T, k_ratio
    
        zt_help = 300.0/temp
        k0_T = k0_300K * zt_help**n * cair
        kinf_T = kinf_300K * zt_help**m
        k_ratio = k0_T/kinf_T
    
        TROE = k0_T / (1.0 + k_ratio) * 0.6**(1.0/(1.0+LOG10(k_ratio)**2))
  END FUNCTION TROE


!----------------------------------- TROEE -----------------------------------
! Troe equilibrium reactions (Stockwell et. al., 1997)                        
!                                                                             
! @param[in]     a0        None                                               
! @param[in]     b0        None                                               
! @param[in]     k0_300K   None                                               
! @param[in]     n         None                                               
! @param[in]     kinf_300K None                                               
! @param[in]     m         None                                               
! @param[in]     temp      Temperature                                        
! @param[in]     cair      None                                               
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION TROEE(a0, b0, k0_300K, n, kinf_300K, m, temp, cair)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: a0
    REAL(8), INTENT(IN) :: b0
    REAL(8), INTENT(IN) :: k0_300K
    REAL(8), INTENT(IN) :: n
    REAL(8), INTENT(IN) :: kinf_300K
    REAL(8), INTENT(IN) :: m
    REAL(8), INTENT(IN) :: temp
    REAL(8), INTENT(IN) :: cair

        REAL(8) :: zt_help, k0_T, kinf_T, k_ratio, troe
    
        zt_help = 300.0/temp
        k0_T = k0_300K * zt_help**n * cair
        kinf_T = kinf_300K * zt_help**m
        k_ratio = k0_T/kinf_T 
        troe = k0_T / (1.0 + k_ratio) * 0.6**(1.0/(1.0+ LOG10(k_ratio)**2))
    
        TROEE = a0 * EXP(-b0 / temp) * troe;
  END FUNCTION TROEE


!---------------------------------- Sunlight ---------------------------------
! Calculates sunlight intensity in the range [0,1] as a function of time.     
! Modify this routine to get the correct sunlight values for your model.      
!                                                                             
! @param[in]     time Integration time                                        
! @param[in]     idx  Current grid cell index                                 
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION Sunlight(time, idx)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: time
    INTEGER, INTENT(IN) :: idx

        REAL, PARAMETER :: PI = 4.0*ATAN(1.0)        ! Pi
        INTEGER, PARAMETER :: daysec = 24 * 3600     ! Seconds per day
        REAL, PARAMETER :: sunrise = 5.5*3600        ! 5:30 local time
        REAL, PARAMETER :: sunset = 19.5*3600        ! 7:30 local time
        REAL :: daily, tmp
        
        daily = time - (INT(time) / daysec) * daysec
        
        ! Estimate sunlight intensity in the range [0,1]
        IF ((daily >= sunrise) .AND. (daily <= sunset)) THEN
            tmp = (2.0 * daily - sunrise - sunset) / (sunset - sunrise);
            IF (tmp > 0) THEN
                tmp = tmp * tmp
            ELSE
                tmp = -tmp * tmp
            END IF
            tmp = 0.5 * (1.0 + COS(PI * tmp))
        ELSE
            tmp = 0.0
        END IF
        Sunlight = tmp
  END FUNCTION Sunlight


!-------------------------------- Temperature --------------------------------
! Calculates temperature (kelvin) as a function of time.                      
! Modify this routine to get the correct temperature values for your model.   
!                                                                             
! @param[in]     time Integration time                                        
! @param[in]     idx  Current grid cell index                                 
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION Temperature(time, idx)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: time
    INTEGER, INTENT(IN) :: idx

        REAL, PARAMETER :: PI = 4.0*ATAN(1.0)   ! Pi
        REAL, PARAMETER :: mintemp = 280        ! 280 Kelvin ~= 44 Fahrenheit
        REAL, PARAMETER :: maxtemp = 300        ! 300 Kelvin ~= 80 Fahrenheit
        REAL :: tmp
    
        ! Estimate temperature cycling from mintemp to maxtemp
        tmp = SIN(time*PI/(24*3600))
        IF (tmp < 0) THEN
            tmp = mintemp - tmp * (maxtemp-mintemp)
        ELSE
            tmp = mintemp + tmp * (maxtemp-mintemp)
        END IF
        Temperature = tmp
  END FUNCTION Temperature


!----------------------------------- Rates -----------------------------------
! Calculates reaction rate coefficients                                       
!                                                                             
! @param[in]     time Integration time                                        
! @param[in]     idx  Current grid cell index                                 
! @param[out]    rct  Reaction rates                                          
!-----------------------------------------------------------------------------
  SUBROUTINE Rates(time, idx, rct, CHI)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: time
    INTEGER, INTENT(IN) :: idx
    REAL(8), INTENT(IN) :: CHI
    REAL(8), INTENT(OUT) :: rct(193)

    ! Sunlight intensity: 0 to 1 inclusive (uppercase for KPP compatibility) 
    REAL(8) :: SUN
    ! Temperature in kelvin  (uppercase for KPP compatibility) 
    REAL(8) :: TEMP

!!$    SUN = Sunlight(time, idx)
!!$    TEMP = Temperature(time, idx)
    TEMP = 279.907043E+00;

    rct(1) = PHUX(1.07E-02,1.01319E+00,0.83330E+00,CHI)
    rct(2) = PHUX(3.22E-05,4.45037E+00,0.78028E+00,CHI)
    rct(3) = PHUX(5.36E-04,0.34764E+00,0.91030E+00,CHI)
    rct(4) = PHUX(8.96E-04,0.99438E+00,0.83295E+00,CHI)
    rct(5) = PHUX(5.48E-07,2.86922E+00,0.79561E+00,CHI)
    rct(6) = PHUX(3.90E-06,2.37354E+00,0.79830E+00,CHI)+EQT2(TEMP)
    rct(7) = PHUX(2.74E-02,0.26226E+00,0.92849E+00,CHI)
    rct(8) = PHUX(2.73E-01,0.29327E+00,0.92401E+00,CHI)
    rct(9) = PHUX(7.78E-06,1.91463E+00,0.79810E+00,CHI)
    rct(10) = PHUX(4.92E-05,1.60973E+00,0.80184E+00,CHI)
    rct(11) = PHUX(4.05E-05,2.06917E+00,0.80267E+00,CHI)
    rct(12) = PHUX(5.40E-06,2.52915E+00,0.79722E+00,CHI)
    rct(13) = PHUX(6.37E-06,1.76570E+00,0.80004E+00,CHI)
    rct(14) = PHUX(6.37E-06,1.76570E+00,0.80004E+00,CHI)
    rct(15) = PHUX(6.10E-09,9.17009E+00,0.72585E+00,CHI)
    rct(16) = PHUX(1.32E-05,2.46350E+00,0.79768E+00,CHI)
    rct(17) = PHUX(3.11E-03,0.55016E+00,0.88313E+00,CHI)
    rct(18) = PHUX(3.11E-03,0.55016E+00,0.88313E+00,CHI)
    rct(19) = PHUX(1.85E-03,0.57967E+00,0.87921E+00,CHI)
    rct(20) = PHUX(6.39E-04,1.53712E+00,0.80233E+00,CHI)
    rct(21) = PHUX(7.20E-08,9.11436E+00,0.72600E+00,CHI)
    rct(22) = 2.55e19 * 6.0E-34 * (TEMP/300.)**(-2.3)
    rct(23) = 6.50E-12 * exp( 120. / TEMP )
    rct(24) = 2.00E-11 * exp( 130. / TEMP )
    rct(25) = 3.20E-11 * exp( 67. / TEMP )
    rct(26) = 2.14e-10
    rct(27) = 1.4E-12 * exp( -1310. / TEMP )
    rct(28) = 1.70E-12 * exp( -940. / TEMP )
    rct(29) = 1.10E-14 * exp( -500. / TEMP )
    rct(30) = 3.45E-12 * exp( 270. / TEMP )
    rct(31) = TROE2( 1.8E-31, 3.2e0, 4.7E-12, 1.4e0, TEMP )
    rct(32) = 2.2E-13 * exp(620./TEMP) + 1.9E-33 * 2.55e19 * exp(980./TEMP)
    rct(33) = 3.08e-34*exp(2820./TEMP)+2.66e-54*2.55e19*exp(3180./TEMP)
    rct(34) = 3.30E-12 * exp( -200. / TEMP )
    rct(35) = TROE2( 7.0e-31, 2.6e0, 1.5e-11, 0.5e0, TEMP )
    rct(36) = 3.30E-39 * exp( 530. / TEMP )
    rct(37) = 1.40E-13 * exp( -2470. / TEMP )
    rct(38) = 1.80E-11 * exp( 110. / TEMP )
    rct(39) = 2.50E-14 * exp( -1230. / TEMP )
    rct(40) = 2.5e-12
    rct(41) = TROE2( 2.2e-30, 4.3e0, 1.5e-12, 0.5e0, TEMP )
    rct(42) = EQT(2.2e-30,4.3e0,1.5e-12,0.5e0,TEMP,9.09e+26,11200.e0)
    rct(43) = 0.0
    rct(44) = TROE2( 2.6e-30, 3.2e0, 2.4e-11, 1.3e0, TEMP )
    rct(45) = SPEZ(7.2e-15,785.e0,4.1e-16,1440.e0,1.9e-33,725.e0,TEMP)
    rct(46) = 1.30E-12 * exp( 380. / TEMP )
    rct(47) = 4.80E-11 * exp( 250. / TEMP )
    rct(48) = TROE2( 3.0e-31, 3.3e0, 1.5e-12, 0.0e0, TEMP )
    rct(49) = 2.4329175e-13
    rct(50) = TEMP * TEMP * 6.95E-18 * exp( -1280. / TEMP )
    rct(51) = TEMP * TEMP * 1.37E-17 * exp( -444. /TEMP )
    rct(52) = 1.59E-11 * exp( -540. / TEMP )
    rct(53) = 1.73E-11 * exp( -380. / TEMP )
    rct(54) = 3.64E-11 * exp( -380. / TEMP )
    rct(55) = 2.15E-12 * exp( 411. / TEMP )
    rct(56) = 5.32E-12 * exp( 504. / TEMP )
    rct(57) = 1.07E-11 * exp( 549. / TEMP )
    rct(58) = 2.10E-12 * exp( 322. / TEMP )
    rct(59) = 1.89E-11 * exp( 116. / TEMP )
    rct(60) = 4e-11
    rct(61) = 9e-12
    rct(62) = 6.87E-12 * exp( 256. / TEMP )
    rct(63) = 1.20E-11 * exp( -745. / TEMP )
    rct(64) = 1.15e-11
    rct(65) = 1.7e-11
    rct(66) = 2.8e-11
    rct(67) = 1e-11
    rct(68) = 1e-11
    rct(69) = 1e-11
    rct(70) = TEMP * TEMP *6.85E-18 * exp( -444. / TEMP )
    rct(71) = 1.55E-11 * exp( -540. / TEMP )
    rct(72) = 2.55E-11 * exp( 409. / TEMP )
    rct(73) = 2.6E-12 * exp ( 380. / TEMP)
    rct(74) = 2.E16 * exp (-13500. / TEMP)
    rct(75) = 4.7e-12
    rct(76) = 1.95E16 * exp(-13543. / TEMP )
    rct(77) = 4.20E-12 * exp( 180. / TEMP )
    rct(78) = 4.20E-12 * exp( 180. / TEMP )
    rct(79) = 4.20E-12 * exp( 180. / TEMP )
    rct(80) = 4.20E-12 * exp( 180. / TEMP )
    rct(81) = 4.20E-12 * exp( 180. / TEMP )
    rct(82) = 4.20E-12 * exp( 180. / TEMP )
    rct(83) = 4.20E-12 * exp( 180. / TEMP )
    rct(84) = 3.50E-11 * exp( -180. / TEMP )
    rct(85) = 4.20E-12 * exp( 180. / TEMP )
    rct(86) = 4.20E-12 * exp( 180. / TEMP )
    rct(87) = 4.20E-12 * exp( 180. / TEMP )
    rct(88) = 4.20E-12 * exp( 180. / TEMP )
    rct(89) = 4.20E-12 * exp( 180. / TEMP )
    rct(90) = 4.20E-12 * exp( 180. / TEMP )
    rct(91) = 6.00E-13 * exp( -2058. / TEMP )
    rct(92) = 1.40E-12 * exp( -1900. / TEMP)
    rct(93) = 6.00E-13 * exp( -2058. / TEMP )
    rct(94) = 1.40E-12 * exp( -1900. / TEMP)
    rct(95) = 1.40E-12 * exp( -1900. / TEMP)
    rct(96) = 2.2e-11
    rct(97) = 2.00E-12 * exp( -2923. / TEMP )
    rct(98) = 1.00E-11 * exp( -1895. / TEMP )
    rct(99) = 3.23E-11 * exp( -975. / TEMP )
    rct(100) = 5.81e-13
    rct(101) = 1.20E-14 * exp( -2633. / TEMP )
    rct(102) = 1.32E-14 * exp( -2105. / TEMP )
    rct(103) = 7.29E-15 * exp( -1136. / TEMP )
    rct(104) = 1.23E-14 * exp( -2013. / TEMP )
    rct(105) = 7.70E-14 * exp( 1300. / TEMP )
    rct(106) = 7.70E-14 * exp( 1300. / TEMP )
    rct(107) = 7.70E-14 * exp( 1300. / TEMP )
    rct(108) = 7.70E-14 * exp( 1300. / TEMP )
    rct(109) = 7.70E-14 * exp( 1300. / TEMP )
    rct(110) = 7.70E-14 * exp( 1300. / TEMP )
    rct(111) = 7.70E-14 * exp( 1300. / TEMP )
    rct(112) = 7.70E-14 * exp( 1300. / TEMP )
    rct(113) = 7.70E-14 * exp( 1300. / TEMP )
    rct(114) = 7.70E-14 * exp( 1300. / TEMP )
    rct(115) = 7.70E-14 * exp( 1300. / TEMP )
    rct(116) = 7.70E-14 * exp( 1300. / TEMP )
    rct(117) = 7.70E-14 * exp( 1300. / TEMP )
    rct(118) = 7.70E-14 * exp( 1300. / TEMP )
    rct(119) = 1.90E-13 * exp( 220. / TEMP )
    rct(120) = 1.40E-13 * exp( 220. / TEMP )
    rct(121) = 4.20E-14 * exp( 220. / TEMP )
    rct(122) = 3.40E-14 * exp( 220. / TEMP )
    rct(123) = 2.90E-14 * exp( 220. / TEMP )
    rct(124) = 1.40E-13 * exp( 220. / TEMP )
    rct(125) = 1.40E-13 * exp( 220. / TEMP )
    rct(126) = 1.70E-14 * exp( 220. / TEMP )
    rct(127) = 1.70E-14 * exp( 220. / TEMP )
    rct(128) = 9.60E-13 * exp( 220. / TEMP )
    rct(129) = 1.70E-14 * exp( 220. / TEMP )
    rct(130) = 1.70E-14 * exp( 220. / TEMP )
    rct(131) = 9.60E-13 * exp( 220. / TEMP )
    rct(132) = 3.40E-13 * exp( 220. / TEMP )
    rct(133) = 1.00E-13 * exp( 220. / TEMP )
    rct(134) = 8.40E-14 * exp( 220. / TEMP )
    rct(135) = 7.20E-14 * exp( 220. / TEMP )
    rct(136) = 3.40E-13 * exp( 220. / TEMP )
    rct(137) = 3.40E-13 * exp( 220. / TEMP )
    rct(138) = 4.20E-14 * exp( 220. / TEMP )
    rct(139) = 4.20E-14 * exp( 220. / TEMP )
    rct(140) = 1.19E-12 * exp( 220. / TEMP )
    rct(141) = 4.20E-14 * exp( 220. / TEMP )
    rct(142) = 4.20E-14 * exp( 220. / TEMP )
    rct(143) = 1.19E-12 * exp( 220. / TEMP )
    rct(144) = 7.70E-14 * exp( 1300. / TEMP )
    rct(145) = 1.70E-14 * exp( 220. / TEMP )
    rct(146) = 4.20E-14 * exp( 220. / TEMP )
    rct(147) = 4.20E-12 * exp( 180. / TEMP )
    rct(148) = 4.20E-12 * exp( 180. / TEMP )
    rct(149) = 7.70E-14 * exp( 1300. / TEMP )
    rct(150) = 1.70E-14 * exp( 220. / TEMP )
    rct(151) = 4.20E-14 * exp( 220. / TEMP )
    rct(152) = 1.70E-14 * exp( 220. / TEMP )
    rct(153) = 4.20E-14 * exp( 220. / TEMP )
    rct(154) = 3.60E-16 * exp( 220. / TEMP )
    rct(155) = 1.21E-11 * exp( 444. / TEMP )
    rct(156) = ARR2(1.19E-12,490.,TEMP)
    rct(157) = ARR2(1.01E-15,-736.,TEMP)
    rct(158) = 4e-12
    rct(159) = 1.5e-11
    rct(160) = ARR2(3.56E-14,708.,TEMP)
    rct(161) = ARR2(7.40E-13,765.,TEMP)
    rct(162) = 1.2e-12
    rct(163) = 1.7e-10
    rct(164) = 1.22e-11
    rct(165) = 2e-16
    rct(166) = 4e-12
    rct(167) = 1.5e-11
    rct(168) = ARR2(3.56E-14,708.,TEMP)
    rct(169) = ARR2(7.40E-13,765.,TEMP)
    rct(170) = 1.2e-12
    rct(171) = ARR2(2.43E-12,360.,TEMP)
    rct(172) = ARR2(2.05E-13,1300.,TEMP)
    rct(173) = 2e-12
    rct(174) = 1e-10
    rct(175) = 1.3e-11
    rct(176) = 0.5*(4.13E-12 * exp( 452. / TEMP ) + 1.86E-11 * exp( 175. / &
        TEMP ))
    rct(177) = 0.5*(1.36E-15 * exp( -2112. / TEMP ) + 7.51E-16 * exp( -1521. &
        / TEMP ))
    rct(178) = ARR2(2.54E-12,360.,TEMP)
    rct(179) = ARR2(1.82E-13,1300.,TEMP)
    rct(180) = 2e-12
    rct(181) = TROE2( 9.7e-29, -5.6e0, 9.3e-12, -1.5e0, TEMP )
    rct(182) = TROE2( 9.7e-29, -5.6e0, 9.3e-12, -1.5e0, TEMP &
        )/(ARR2(9.0E-19,14000.,TEMP))
    rct(183) = 3.6e-12
    rct(184) = 3e-11
    rct(185) = 3e-12
    rct(186) = ARR2(5.60E-12,270.,TEMP)
    rct(187) = ARR2(1.9E-13,500.,TEMP)
    rct(188) = ARR2(9.6E-12,-234.,TEMP)
    rct(189) = &
        ARR2(3.04E-2,350.,TEMP)*ARR2(1.106E-31,7460.,TEMP)*2.55e19/(1+ARR2(1.106E-31,7460.,TEMP)*2.55e19)
    rct(190) = 5.8e-11
    rct(191) = 2.5e-12
    rct(192) = 2.5e-12
    rct(193) = 2.5e-12
  END SUBROUTINE Rates



END MODULE box_model_rates
!------------------------ END box_model_rates.f90 END ------------------------

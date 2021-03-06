!------------------------ BEGIN small_rates.f90 BEGIN ------------------------
! @file small_rates.f90                                                       
! @author jlinford                                                            
! @date 2014-06-01 10:26:33.169896                                            
! @brief Reaction rate calculation and utility functions                      
!                                                                             
! Reaction rate calculation and utility functions                             
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE small_rates

  USE small_parameters

  IMPLICIT NONE





  CONTAINS

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

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Update_SUN - update SUN light using TIME
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE Update_SUN(TIME, SUN)
    IMPLICIT NONE

    REAL(8) :: SunRise, SunSet
    REAL(8) :: Thour, Tlocal, Ttmp 
    REAL(8), INTENT(IN) :: TIME
    REAL(8), INTENT(OUT) :: SUN
    ! PI - Value of pi
    REAL(8), PARAMETER :: PI = 3.14159265358979d0
    
    SunRise = 4.5
    SunSet  = 19.5
    Thour = TIME/3600.0
    Tlocal = Thour - (INT(Thour)/24)*24

    IF ((Tlocal>=SunRise).AND.(Tlocal<=SunSet)) THEN
       Ttmp = (2.0*Tlocal-SunRise-SunSet)/(SunSet-SunRise)
       IF (Ttmp.GT.0) THEN
          Ttmp =  Ttmp*Ttmp
       ELSE
          Ttmp = -Ttmp*Ttmp
       END IF
       SUN = ( 1.0 + COS(PI*Ttmp) )/2.0
    ELSE
       SUN = 0.0
    END IF

 END SUBROUTINE Update_SUN


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
    REAL(8) :: SUN

    CALL Update_SUN(time, SUN)
    Sunlight = SUN

!        REAL, PARAMETER :: PI = 4.0*ATAN(1.0)        ! Pi
!        INTEGER, PARAMETER :: daysec = 24 * 3600     ! Seconds per day
!        REAL, PARAMETER :: sunrise = 5.5*3600        ! 5:30 local time
!        REAL, PARAMETER :: sunset = 19.5*3600        ! 7:30 local time
!        REAL :: daily, tmp
!        
!        daily = time - (INT(time) / daysec) * daysec
!        
!        ! Estimate sunlight intensity in the range [0,1]
!        IF ((daily >= sunrise) .AND. (daily <= sunset)) THEN
!            tmp = (2.0 * daily - sunrise - sunset) / (sunset - sunrise);
!            IF (tmp > 0) THEN
!                tmp = tmp * tmp
!            ELSE
!                tmp = -tmp * tmp
!            END IF
!            tmp = 0.5 * (1.0 + COS(PI * tmp))
!        ELSE
!            tmp = 0.0
!        END IF
!        Sunlight = tmp
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

    Temperature = 270
!        REAL, PARAMETER :: PI = 4.0*ATAN(1.0)   ! Pi
!        REAL, PARAMETER :: mintemp = 280        ! 280 Kelvin ~= 44 Fahrenheit
!        REAL, PARAMETER :: maxtemp = 300        ! 300 Kelvin ~= 80 Fahrenheit
!        REAL :: tmp
!    
!        ! Estimate temperature cycling from mintemp to maxtemp
!        tmp = SIN(time*PI/(24*3600))
!        IF (tmp < 0) THEN
!            tmp = mintemp - tmp * (maxtemp-mintemp)
!        ELSE
!            tmp = mintemp + tmp * (maxtemp-mintemp)
!        END IF
!        Temperature = tmp
  END FUNCTION Temperature


!----------------------------------- Rates -----------------------------------
! Calculates reaction rate coefficients                                       
!                                                                             
! @param[in]     time Integration time                                        
! @param[in]     idx  Current grid cell index                                 
! @param[out]    rct  Reaction rates                                          
!-----------------------------------------------------------------------------
  SUBROUTINE Rates(time, idx, rct)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: time
    INTEGER, INTENT(IN) :: idx
    REAL(8), INTENT(OUT) :: rct(10)

    ! Sunlight intensity: 0 to 1 inclusive (uppercase for KPP compatibility) 
    REAL(8) :: SUN
    ! Temperature in kelvin  (uppercase for KPP compatibility) 
    REAL(8) :: TEMP

    SUN = Sunlight(time, idx)
    TEMP = Temperature(time, idx)

    rct(1) = (2.643E-10) * SUN*SUN*SUN
    rct(2) = 8.018e-17
    rct(3) = (6.120E-04) * SUN
    rct(4) = 1.576e-15
    rct(5) = (1.070E-03) * SUN*SUN
    rct(6) = 7.11e-11
    rct(7) = 1.2e-10
    rct(8) = 6.062e-15
    rct(9) = 1.069e-11
    rct(10) = (1.289E-02) * SUN
  END SUBROUTINE Rates



END MODULE small_rates
!-------------------------- END small_rates.f90 END --------------------------

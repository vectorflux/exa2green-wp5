! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Main Program File
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
! File                 : small_f90_Main.f90
! Time                 : Sun Jun  1 10:17:02 2014
! Working directory    : /Users/jlinford/workspace/kppa/verify/small_kpp221
! Equation file        : small_f90.kpp
! Output root filename : small_f90
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! MAIN - Main program - driver routine
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROGRAM small_f90_Driver

  USE small_f90_Model
  USE small_f90_Initialize, ONLY: Initialize

      REAL(kind=dp) :: T, DVAL(NSPEC)
      REAL(kind=dp) :: RSTATE(20)
      INTEGER :: i
  
!~~~> Initialization 

      STEPMIN = 0.0d0
      STEPMAX = 0.0d0

      DO i=1,NVAR
        RTOL(i) = 1.0d-4
        ATOL(i) = 1.0d-3
      END DO
     
      CALL Initialize()
      CALL InitSaveData()

!~~~> Time loop
      T = TSTART
kron: DO WHILE (T < TEND)

        TIME = T
        CALL GetMass( C, DVAL )
        WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,       &
                   ( TRIM(SPC_NAMES(MONITOR(i))),           &
                     C(MONITOR(i))/CFACTOR, i=1,NMONITOR ), &
                   ( TRIM(SMASS(i)), DVAL(i)/CFACTOR, i=1,NMASS )
        CALL SaveData()
        CALL Update_SUN() 
        CALL Update_RCONST()

        CALL INTEGRATE( TIN = T, TOUT = T+DT, RSTATUS_U = RSTATE, &
        ICNTRL_U = (/ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /) )
        T = RSTATE(1)

      END DO kron
!~~~> End Time loop

      CALL GetMass( C, DVAL )
      WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,     &
               ( TRIM(SPC_NAMES(MONITOR(i))),           &
                 C(MONITOR(i))/CFACTOR, i=1,NMONITOR ), &
               ( TRIM(SMASS(i)), DVAL(i)/CFACTOR, i=1,NMASS )
      TIME = T
      CALL SaveData()
      CALL CloseSaveData()

991   FORMAT(F6.1,'%. T=',E9.3,2X,200(A,'=',E11.4,'; '))

END PROGRAM small_f90_Driver

! End of MAIN function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



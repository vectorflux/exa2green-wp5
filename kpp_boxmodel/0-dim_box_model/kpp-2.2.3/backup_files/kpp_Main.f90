! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Main Program File
! 
! Generated by KPP-2.2.3_rs5 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : kpp_Main.f90
! Time                 : Mon Dec  1 09:16:28 2014
! Working directory    : /users/charlesj/KPP_BOXMODEL/0-dim_box_model/kpp-2.2.3
! Equation file        : kpp.kpp
! Output root filename : kpp
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! MAIN - Main program - driver routine
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROGRAM kpp_Driver

  USE kpp_Model
  USE kpp_Initialize, ONLY: Initialize
  USE kpp_Parameters, ONLY: idim, jdim, kdim, idim_spot, jdim_spot, kdim_spot, &
       idim_loc_s, jdim_loc_s, kdim_loc_s, idim_loc_e, jdim_loc_e, kdim_loc_e, &
       NMONITOR, TAG, ref_sol, ncells
  USE omp_lib

  INCLUDE "mpif.h"

      REAL(kind=dp) :: T, DVAL(NSPEC)
      REAL(kind=dp) :: RSTATE(20), RCNTRL(20)
      REAL(kind=dp) :: TTS, TTS_init, TTS_final
      REAL(kind=dp) :: ETS, ETS_init, ETS_final
      REAL(kind=dp) :: device_ETS, device_ETS_init, device_ETS_final
      REAL(kind=dp) :: ETS_init_total, ETS_final_total
      REAL(kind=dp) :: device_ETS_init_total, device_ETS_final_total
      REAL(kind=dp) :: TTS_first_call, ETS_first_call, device_ETS_first_call
      REAL(kind=dp) :: rel_acc, weight

      INTEGER :: ISTATE(20), ICNTRL(20)
      INTEGER :: i, nbit, ii, jj, kk
      INTEGER :: tsteps_first_call

!~~~> Initialization 

      ! get energy counter at startup
      CALL energy(ETS_init_total)
      CALL device_energy(device_ETS_init_total)

      DO i = 1, 20
         ISTATE(i) = 0
         ICNTRL(i) = 0
         RSTATE(i) = 0.0d0
         RCNTRL(i) = 0.0d0
      END DO

      ICNTRL(2) = 1
      ICNTRL(3) = 3

      STEPMIN = 0.0d0
      STEPMAX = 0.0d0
      
      TTS = 0.0d0
      ETS = 0.0d0
      device_ETS = 0.0d0
      TTS_first_call = 0.0d0
      ETS_first_call = 0.0d0
      device_ETS_first_call = 0.0d0

      nbit = 0
      tsteps_first_call = 0
      global_ISTATS(:) = 0

      DO i=1,NVAR
        RTOL(i) = 1.0d-2
        ATOL(i) = 1.0d-2
      END DO

      rel_acc = 0.0D0
      weight = 0.0D0

      CALL Initialize()
      CALL InitSaveData()

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(kk,jj,ii)
      !$OMP DO COLLAPSE(2)
      DO kk=1,kdim
         DO jj=1,jdim
            DO ii=1,idim
               ! set initial values with those of box(ii,jj,kk)
               VARTOT(1:NVAR,ii,jj,kk) = C(1:NVAR) * 1000
            ENDDO
         ENDDO
      ENDDO
      !$OMP END DO 
      !$OMP END PARALLEL

      CALL OMP_SET_DYNAMIC(.FALSE.)

!~~~> Time loop
      T = TSTART
kron: DO WHILE (T < TEND)

        nbit = nbit + 1

        ISTATS(:) = 0

        TIME = T

        WRITE(6,990) (T-TSTART)/(TEND-TSTART)*100, T,       &
                   ( TRIM(SPC_NAMES(MONITOR(i))),           &
                     VARTOT(MONITOR(i),idim_spot,jdim_spot,kdim_spot)/CFACTOR, i=1,NMONITOR )
        CALL SaveData()

        ! get energy counter at startup
        CALL energy(ETS_init)
        CALL device_energy(device_ETS_init)

        TTS_init = MPI_WTIME()

        CHI = CHIBE( T/60.0d+00, 48.0d+00, 2.0d+00, TAG )

        CALL GetMass( C, DVAL )

        !CALL Update_SUN() 
        CALL Update_RCONST()

        CALL INTEGRATE( TIN = T, TOUT = T+DT, ISTATUS_U = ISTATE, &
             RSTATUS_U = RSTATE, ICNTRL_U = ICNTRL, RCNTRL_U = RCNTRL)

        TTS_final = MPI_WTIME()

        ! get energy counter at end
        CALL energy(ETS_final)
        CALL device_energy(device_ETS_final)

        TTS = TTS + (TTS_final - TTS_init)
        ETS = ETS + (ETS_final - ETS_init)
        device_ETS = device_ETS + (device_ETS_final - device_ETS_init)

        IF (nbit == 1) THEN
           TTS_first_call = TTS
           ETS_first_call = ETS
           device_ETS_first_call = device_ETS
           tsteps_first_call = ISTATS(3)
        END IF

        global_ISTATS(:) = global_ISTATS(:) + ISTATS(:)

        T = RSTATE(1)

      END DO kron
!~~~> End Time loop

      CALL GetMass( C, DVAL )

      WRITE(6,990) (T-TSTART)/(TEND-TSTART)*100, T,     &
               ( TRIM(SPC_NAMES(MONITOR(i))),           &
                 VARTOT(MONITOR(i),idim_spot,jdim_spot,kdim_spot)/CFACTOR, i=1,NMONITOR )
  
      TIME = T
      CALL SaveData()
      CALL CloseSaveData()

      ! get energy counter at end
      CALL energy(ETS_final_total)
      CALL device_energy(device_ETS_final_total)

      i = 1
      DO WHILE (i < NMONITOR + 1)
         rel_acc = rel_acc + (ref_sol(i) - VARTOT(MONITOR(i),idim_spot,jdim_spot,kdim_spot)/CFACTOR)**2
         weight = weight + ref_sol(i)**2
         i = i + 1
      END DO
      rel_acc = SQRT(rel_acc/weight)

      WRITE(6,*)
      WRITE(6,992) 'Total Rosenbrock function calls                 = ', global_ISTATS(Nfun)
      WRITE(6,992) 'Total Rosenbrock jacobian calls                 = ', global_ISTATS(Njac)
      WRITE(6,992) 'Total Rosenbrock steps                          = ', global_ISTATS(Nstp)
      WRITE(6,992) 'Total Rosenbrock accepted steps                 = ', global_ISTATS(Nacc)
      WRITE(6,992) 'Total Rosenbrock rejected steps                 = ', global_ISTATS(Nrej)
      WRITE(6,992) 'Total Rosenbrock LU decompositions              = ', global_ISTATS(Ndec)
      WRITE(6,992) 'Total Rosenbrock forward/backward substitutions = ', global_ISTATS(Nsol)
      WRITE(6,992) 'Total Rosenbrock singular matrix decompositions = ', global_ISTATS(Nsng)
      WRITE(6,*)
      WRITE(6,999) 'Absolute Tolerance = ', ATOL(1)
      WRITE(6,999) 'Relative Tolerance = ', RTOL(1)
      WRITE(6,*)
      WRITE(6,995) 'ICNTRL = ', ICNTRL
      WRITE(6,996) 'RCNTRL = ', RCNTRL
      WRITE(6,*)
      WRITE(6,998) 'Subgrid coordinates (lon.)  = ', idim_loc_s, idim_loc_e
      WRITE(6,998) 'Subgrid coordinates (lat.)  = ', jdim_loc_s, jdim_loc_e
      WRITE(6,998) 'Subgrid coordinates (vert.) = ', kdim_loc_s, kdim_loc_e
      WRITE(6,*)
      WRITE(6,992) 'Total number of grid points = ', ncells
      WRITE(6,*)
      WRITE(6,*)   '******** First Timestep (average of all cells) ********'
      WRITE(6,992) 'Rosenbrock steps = ', INT(tsteps_first_call/ncells)
      WRITE(6,993) 'TTS              = ', TTS_first_call, ' s'
      WRITE(6,994) 'ETS              = ', ETS_first_call, ' J'
      WRITE(6,994) 'Device ETS       = ', device_ETS_first_call, ' J'
      WRITE(6,*)
      WRITE(6,*)   '******** One Timestep (average of all cells) ********'
      WRITE(6,992) 'Rosenbrock steps = ', INT(global_ISTATS(Nstp)/(ncells*nbit))
      WRITE(6,993) 'TTS              = ', TTS/nbit, ' s'
      WRITE(6,994) 'ETS              = ', ETS/nbit, ' J'
      WRITE(6,994) 'Device ETS       = ', device_ETS/nbit, ' J'
      WRITE(6,*)
      WRITE(6,991) 'Total number of timesteps = ', nbit 
      WRITE(6,*)
      WRITE(6,*)   '******** Total Timesteps ********'
      WRITE(6,992) 'Rosenbrock steps = ', INT(global_ISTATS(Nstp)/ncells)
      WRITE(6,993) 'TTS              = ', TTS, ' s'
      WRITE(6,994) 'ETS              = ', ETS, ' J'
      WRITE(6,994) 'Device ETS       = ', device_ETS, ' J'
      WRITE(6,*)
      WRITE(6,*)   '******** Entire application ********'
      WRITE(6,994) 'ETS              = ', ETS_final_total-ETS_init_total, ' J'
      WRITE(6,994) 'Device ETS       = ', device_ETS_final_total-device_ETS_init_total, ' J'
      WRITE(6,*)
      WRITE(6,997) 'Relative accuracy to ref. sol. = ', rel_acc 
      WRITE(6,*)

990   FORMAT(F6.1,'%. T=',E9.3,2X,200(A,'=',E11.4,'; '))
991   FORMAT(A,I4)
992   FORMAT(A,I11)
993   FORMAT(A,F11.3,A)
994   FORMAT(A,F11.1,A)
995   FORMAT(A,20(I7,","))
996   FORMAT(A,20(F7.2,","))
997   FORMAT(A,E11.4)
998   FORMAT(A,I5,I5)
999   FORMAT(A,F6.2)

!==============================================================================
CONTAINS
!==============================================================================

  SUBROUTINE device_energy(e)
    IMPLICIT NONE
    REAL (kind=8), INTENT(out) :: e
    
    OPEN(unit=50, file='/sys/cray/pm_counters/accel_energy' ,action='READ')
    READ(50,*) e
    CLOSE(50)
  END SUBROUTINE device_energy

  SUBROUTINE energy(e)
    IMPLICIT NONE
    REAL (kind=8), INTENT(out) :: e
    
    OPEN(unit=50, file='/sys/cray/pm_counters/energy' ,action='READ')
    READ(50,*) e
    CLOSE(50)
  END SUBROUTINE energy

END PROGRAM kpp_Driver

! End of MAIN function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



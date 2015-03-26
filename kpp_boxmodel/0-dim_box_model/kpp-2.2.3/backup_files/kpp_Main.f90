! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Main Program File
! 
! Generated by KPP-2.2.3 symbolic chemistry Kinetics PreProcessor
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
! Time                 : Wed May 21 15:52:45 2014
! Working directory    : /home/charlesj/Desktop/CHECKOUT/kpp_boxmodel/0-dim_box_model_Exa2Green/kpp-2.2.3_boxmodel
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
  USE kpp_Parameters, ONLY: idim, jdim, kdim, idim_spot, jdim_spot, kdim_spot, NMONITOR, TAG
  USE omp_lib

  INCLUDE "mpif.h"

      REAL(kind=dp) :: T, DVAL(NSPEC)
      REAL(kind=dp) :: RSTATE(20)
      INTEGER :: i, nbit, ii, jj, kk

      REAL(kind=dp) :: starttime, endtime, meantime
      REAL(kind=dp) :: energy_ts_init, energy_ts_final
      REAL(kind=dp) :: device_energy_ts_init, device_energy_ts_final
      REAL(kind=dp) :: mean_device_energy_ts, mean_energy_ts
      REAL(kind=dp) :: energy_init, energy_final
      REAL(kind=dp) :: device_energy_init, device_energy_final

!~~~> Initialization 

      ! get energy counter at startup
      CALL energy(energy_init)
      CALL device_energy(device_energy_init)

      STEPMIN = 0.0d0
      STEPMAX = 0.0d0

      mean_energy_ts = 0.0d0
      mean_device_energy_ts = 0.0d0
      meantime = 0.0d0
      nbit = 0

      DO i=1,NVAR
        RTOL(i) = 1.0d-2
        ATOL(i) = 1.0d-2
      END DO
     
      CALL Initialize()
      CALL InitSaveData()

      !$OMP PARALLEL DEFAULT(SHARED) &
      !$OMP PRIVATE(kk,jj,ii)
      !$OMP DO COLLAPSE(2)
      DO kk=1,kdim
         DO jj=1,jdim
            DO ii=1,idim
               ! set initial values with those of box(ii, jj, kk)
               VARTOT(1:NVAR,ii,jj,kk) = C(1:NVAR)
            ENDDO
         ENDDO
      ENDDO
      !$OMP END DO 
      !$OMP END PARALLEL

      CALL OMP_SET_DYNAMIC(.FALSE.)

!~~~> Time loop
      T = TSTART
kron: DO WHILE (T < TEND)

        ! get energy counter at startup
        CALL energy(energy_ts_init)
        CALL device_energy(device_energy_ts_init)

        starttime = MPI_WTIME()

        CHI = CHIBE( T/60.0d+00, 48.0d+00, 2.0d+00, TAG )

        TIME = T
        CALL GetMass( C, DVAL )
        WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,       &
             ( TRIM(SPC_NAMES(MONITOR(i))), VARTOT(MONITOR(i),idim_spot,jdim_spot,kdim_spot)/CFACTOR, i=1,NMONITOR )

        CALL SaveData()
        !CALL Update_SUN() 
        CALL Update_RCONST()

        CALL INTEGRATE( TIN = T, TOUT = T+DT, RSTATUS_U = RSTATE, &
        ICNTRL_U = (/ 0,1,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 /) )

        T = RSTATE(1)

        endtime = MPI_WTIME()
        meantime = meantime + endtime - starttime
        nbit = nbit + 1

        ! get energy counter at end
        CALL energy(energy_ts_final)
        CALL device_energy(device_energy_ts_final)

        mean_energy_ts = mean_energy_ts + energy_ts_final - energy_ts_init
        mean_device_energy_ts = mean_device_energy_ts + &
             device_energy_ts_final - device_energy_ts_init

      END DO kron
!~~~> End Time loop

      WRITE(6,991) (T-TSTART)/(TEND-TSTART)*100, T,     &
           ( TRIM(SPC_NAMES(MONITOR(i))), VARTOT(MONITOR(i),idim_spot,jdim_spot,kdim_spot)/CFACTOR, i=1,NMONITOR )
      TIME = T
      CALL SaveData()
      CALL CloseSaveData()

      ! get energy counter at end
      CALL energy(energy_final)
      CALL device_energy(device_energy_final)

      WRITE(6,*) 'Total elapsed time (timestep) = ', meantime / nbit
      WRITE(6,*) 'Energy (timestep) = ', mean_energy_ts / nbit, ' Joules'
      WRITE(6,*) 'Device energy (timestep) = ', &
           mean_device_energy_ts / nbit, ' Joules'
      WRITE(6,*) 'at rate: ', &
           (mean_energy_ts)/(meantime), ' Watts'
      WRITE(6,*) 
      WRITE(6,*) 'Nb iterations = ', nbit 
      WRITE(6,*) 
      WRITE(6,*) 'Energy = ', energy_final-energy_init, ' Joules'
      WRITE(6,*) 'Device energy = ', device_energy_final - device_energy_init, ' Joules'

991   FORMAT(F6.1,'%. T=',E9.3,2X,200(A,'=',E11.4,'; '))

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



!----------------------- BEGIN box_model_main.f90 BEGIN ----------------------
! @file box_model_main.f90                                                    
! @author jlinford                                                            
! @date 2014-06-03 14:55:36.321254                                            
! @brief Fortran90 driver with plplot                                         
!                                                                             
! Fortran90 driver with plplot                                                
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------



PROGRAM main

    USE box_model_parameters
    USE box_model_integrate
    USE box_model_initialize
    USE box_model_driver_parameters
    USE box_model_monitor

    IMPLICIT NONE

    !---------------------------------------------------------------------------
    ! Lookat data                                                               
    !---------------------------------------------------------------------------
    
    INTEGER, PARAMETER :: lookatFile = 100
    CHARACTER(80), PARAMETER :: fname = "box_model.dat"
    

    !---------------------------------------------------------------------------
    ! Program data                                                               
    !---------------------------------------------------------------------------
    
  ! Integration status code 
  INTEGER :: retval
  ! Integration status code per timestep iteration 
  INTEGER :: tsretval
  ! Integration time 
  REAL(8) :: time

  ! Absolute integration tolerances for variable species 
  REAL(8) :: abstol(81)
  ! Relative integration tolerances for variable species 
  REAL(8) :: reltol(81)

  ! Integer integration in/out parameters 
  INTEGER :: idata(20)
  ! Real value integration in/out parameters 
  REAL(8) :: rdata(20)

    ! Species concentrations for all grid cells
    REAL(8), ALLOCATABLE :: conc(:,:)

    ! Last stepsize in each grid cell
    REAL(8), ALLOCATABLE :: lastH(:)
    
    ! Integrator statistics
    INTEGER :: nFun = 0       ! Number of function evaluations
    INTEGER :: nJac = 0       ! Number of Jacobian evaluations
    INTEGER :: nStp = 0       ! Number of solver steps
    INTEGER :: nAcc = 0       ! Number of accepted steps
    INTEGER :: nRej = 0       ! Number of rejected steps
    INTEGER :: nDec = 0       ! Number of matrix decompositions
    INTEGER :: nSol = 0       ! Number of Ax=b solves
    INTEGER :: nSng = 0       ! Number of singular decomposition results
    
    ! Iterator
    INTEGER :: i
   
    !---------------------------------------------------------------------------
    ! Program text                                                               
    !---------------------------------------------------------------------------

    WRITE(*,101) NCELLS,TSTART,TEND
101 FORMAT("Kppa: Integrating ",I8," grid cells, time interval [",E8.2,",",E8.2,"]")

    ! Initialize example grid data
    ALLOCATE(conc(NSPEC,NCELLS))
    DO i=1,ncells
        CALL Initialize(conc(1:NVAR,i), conc(NVAR+1:NSPEC,i))
    END DO
    
    ! Allocate storage for last step size (useful in reentrant solvers)
    ALLOCATE(lastH(ncells))

    ! Set time interval
    time = TSTART

    ! Initialize vector tolerances
    abstol = ATOLS
    reltol = RTOLS

    ! Initialize integration parameters to 0 = use defaults
    idata = 0
    rdata = 0

    ! Rosenbrock default parameters
    idata(1) = 0       ! System is non-autonomous: F = F(t,y)
    idata(2) = 0       ! Use vector tolerances
    idata(3) = 100000  ! Maximum number of integration steps
    idata(4) = 5       ! Rodas4 Rosenbrock method
    idata(5) = 0       ! Tolerance vectors will not be checked

    rdata(1) = 0       ! Integration step size lower bound: 0 recommended
    rdata(2) = 0       ! Integration step size upper bound: 0 recommended
    rdata(3) = TDEL    ! Starting integration step size
    rdata(4) = 0.2     ! Lower bound on step decrease factor
    rdata(5) = 6       ! Upper bound on step increase factor
    rdata(6) = 0.1     ! Step decrease factor after step rejection
    rdata(7) = 0.9     ! Safety factor in the computation of new step size

    ! Initialize LookAt file
    OPEN(unit=lookatFile,file=fname)

    ! Initialize species monitor
  CALL InitMonitor(rdata(3))



    ! Perform time integration
    retval = 0
    DO WHILE (time < TEND)
        tsretval = GridIntegrate(NCELLS, conc, time, time+TDEL, &
                                 abstol, reltol, idata, rdata, lastH)

        ! Accumulate statistics
        nStp = nStp + idata(13)
        nAcc = nAcc + idata(14)
        nRej = nRej + idata(15)
        nSng = nSng + idata(18) 
        nFun = nFun + idata(11)
        nJac = nJac + idata(12)
        nDec = nDec + idata(16)
        nSol = nSol + idata(17)

        ! Update time
        time = rdata(11)

        ! Write species concentrations to file
      CALL LookAt(rdata(12), rdata(13))

        
        ! Report species concentrations to stdout
      CALL Monitor(rdata(12), rdata(13))

        
        ! Process return code
        IF (tsretval < 0) THEN
            ! Error occured: save code and abort
            retval = MIN(retval, tsretval)
            EXIT
        ELSE IF (tsretval > 0) THEN
            ! Warning occured: save code and continue
            IF (retval >= 0 .AND. retval < tsretval) THEN
                retval = tsretval;
            END IF
        END IF

        ! Use last integration step size as the starting step size
        rdata(3) = rdata(12)
    END DO

    ! Report statistics                       
    WRITE(*,102) nStp,nAcc,nRej,nSng,nFun,nJac,nDec,nSol
102 FORMAT("Kppa: Stp=",I12,", Acc=",I12,", Rej=",I12,", Sng=",I12, &
               ", Fun=",I12,", Jac=",I12,", Dec=",I12,", Sol=",I12)

    ! Clean up and exit
    CLOSE(lookatFile)
    DEALLOCATE(conc)
    DEALLOCATE(lastH)
    CALL EXIT(retval)
    STOP


  CONTAINS

!---------------------------------- Monitor ----------------------------------
! Reports on species concentrations as they are calculated                    
!                                                                             
! @param[in]     step Last accepted integrator time step (H)                  
! @param[in]     err  Error in solution vector                                
!-----------------------------------------------------------------------------
  SUBROUTINE Monitor(step, err)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: step
    REAL(8), INTENT(IN) :: err

        REAL(8) :: perc
        REAL(8) :: cspc(NMONITOR)

        cspc(1) = conc(35,1) / CFACTOR
        cspc(2) = conc(72,1) / CFACTOR
        cspc(3) = conc(76,1) / CFACTOR
        cspc(4) = conc(77,1) / CFACTOR
        cspc(5) = conc(80,1) / CFACTOR

        perc = ((time-TSTART)/ABS(TEND-TSTART))*100.0
        WRITE(*,301) perc,time,step,err,cspc
301     FORMAT(F5.1,"% | ",F9.2," | ",E9.3," | ",E9.3," | ",6(E11.4," |"))
  END SUBROUTINE Monitor



!-------------------------------- InitMonitor --------------------------------
! Initializes the species monitor                                             
!                                                                             
! @param[in]     step Last accepted integrator time step (H)                  
!-----------------------------------------------------------------------------
  SUBROUTINE InitMonitor(step)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: step


        WRITE(*,401)
        WRITE(*,402) "H2O2","HO","NO2","NO","NO3"
        WRITE(*,401)
401     FORMAT(110("-"))
402     FORMAT("       |     T     |     H     | Err. Norm | ",28(A11," |"))


    CALL Monitor(step, ZERO)

  END SUBROUTINE InitMonitor



!----------------------------------- LookAt ----------------------------------
! Writes species concentrations to file                                       
!                                                                             
! @param[in]     step Last accepted integrator time step (H)                  
! @param[in]     err  Error in solution vector                                
!-----------------------------------------------------------------------------
  SUBROUTINE LookAt(step, err)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: step
    REAL(8), INTENT(IN) :: err

    REAL(8) :: data(3 + NLOOKAT)

    data(1) = time
    data(2) = step
    data(3) = err
    data(4) = conc(35, 1) / CFACTOR
    data(5) = conc(72, 1) / CFACTOR
    data(6) = conc(76, 1) / CFACTOR
    data(7) = conc(77, 1) / CFACTOR
    data(8) = conc(80, 1) / CFACTOR
    WRITE(lookatFile,*) data
  END SUBROUTINE LookAt



END PROGRAM main


!------------------------- END box_model_main.f90 END ------------------------
!------------------- BEGIN kppa_OpenMP_integrate.f90 BEGIN -------------------
! @file kppa_OpenMP_integrate.f90                                             
! @author charlesj                                                            
! @date 2014-12-18 14:51:39.282378                                            
! @brief Interface to time stepping integrator                                
!                                                                             
! Definitions of interface functions for the Kppa-generated                   
! time stepping integrator.  These are the Kppa "entry point" routines.       
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE kppa_OpenMP_integrate

  USE kppa_OpenMP_parameters
  USE kppa_OpenMP_rosenbrock

  IMPLICIT NONE




  CONTAINS

!------------------------------- GridIntegrate -------------------------------
! Applies the Kppa-generated integrator to the grid                           
!                                                                             
! @param[in]     ncells Number of grid cells                                  
! @param[in,out] conc   Species concentrations                                
! @param[in]     tstart Integration start time                                
! @param[in]     tend   Integration end time                                  
! @param[in]     abstol Absolute integration tolerances for variable species  
! @param[in]     reltol Relative integration tolerances for variable species  
! @param[in,out] idata  Integer integration in/out parameters                 
! @param[in,out] rdata  Real value integration in/out parameters              
! @param[out]    lastH  Last timestep in each grid cell                       
!-----------------------------------------------------------------------------
  INTEGER FUNCTION GridIntegrate(ncells, conc, tstart, tend, abstol, reltol, &
      idata, rdata, lastH)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncells
    REAL(8), INTENT(INOUT) :: conc(NSPEC*ncells)
    REAL(8), INTENT(IN) :: tstart
    REAL(8), INTENT(IN) :: tend
    REAL(8), INTENT(IN) :: abstol(NVAR)
    REAL(8), INTENT(IN) :: reltol(NVAR)
    INTEGER, INTENT(INOUT) :: idata(20)
    REAL(8), INTENT(INOUT) :: rdata(20)
    REAL(8), INTENT(OUT) :: lastH(ncells)

    ! Return value 
    INTEGER :: retval
    
        INTEGER :: i
        INTEGER :: var
        INTEGER :: fix

        retval = 0

!$OMP  PARALLEL DO & 
!$OMP& DEFAULT(SHARED) &
!$OMP& PRIVATE(i,var,fix) & 
!$OMP& FIRSTPRIVATE(idata, rdata) &
!$OMP& LASTPRIVATE(idata, rdata) &
!$OMP& REDUCTION(IOR:retval)
        DO i = 1, ncells
            var = (i-1)*NSPEC + 1
            fix = var + NVAR
    
            ! Invoke the integrator
            CALL Integrate(conc(var), conc(fix), i, tstart, tend, &
                           abstol, reltol, idata, rdata)
    
            ! Save the last timestep for future use
            lastH(i) = rdata(12)
    
            ! Process integrator return code
            IF (idata(20) < 0) THEN
                WRITE(*,*) "Kppa: CELL",i,"-- INTEGRATION FAILED"
                WRITE(*,*) "Kppa: CELL",i,"idata=",idata
                WRITE(*,*) "Kppa: CELL",i,"rdata=",rdata
                retval = MIN(idata(20), retval)
            ELSE IF (idata(20) > 0) THEN
                WRITE(*,*) "Kppa: CELL",i,"-- INTEGRATION COMPLETED WITH WARNING"
                IF (retval >= 0 .AND. idata(20) > retval) THEN
                    retval = idata(20)
                END IF
            END IF
        END DO
!$OMP END PARALLEL DO

    GridIntegrate = retval
    RETURN
  END FUNCTION GridIntegrate


END MODULE kppa_OpenMP_integrate
!--------------------- END kppa_OpenMP_integrate.f90 END ---------------------
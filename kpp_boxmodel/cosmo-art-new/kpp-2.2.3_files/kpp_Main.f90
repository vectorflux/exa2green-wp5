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
! Time                 : Wed Dec 17 18:11:50 2014
! Working directory    : /home/charlesj/Desktop/CHECKOUT/kpp_boxmodel/cosmo-art-new
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

MODULE kpp_Main

  USE kpp_Model
  USE kpp_Global
  USE kpp_Initialize
  
  IMPLICIT NONE

  REAL(kind=dp), DIMENSION(20) :: r_state
  REAL(kind=dp), DIMENSION(20) :: real_control
  INTEGER, DIMENSION(20) :: i_state
  INTEGER, DIMENSION(20) :: int_control

  CONTAINS

  SUBROUTINE Kpp_Init(n_phot, conc, analysis, i_control, r_control)

    IMPLICIT NONE

    INTEGER, INTENT(in) :: n_phot
    REAL(kind=dp), DIMENSION(NVAR), INTENT(inout) :: conc
    LOGICAL, INTENT(in) :: analysis
    INTEGER, DIMENSION(20), INTENT(in), OPTIONAL :: i_control
    REAL(kind=dp), DIMENSION(20), INTENT(in), OPTIONAL :: r_control
    INTEGER :: i
    
    CALL Initialize()

    !! specify abstol, reltol, stepmin and stepmax here!
DO i=1,NVAR
   ATOL(i)=0.01
   RTOL(i)=0.01
END DO
    NPHOT = n_phot !! number of photolysis rates

    !! ensure that PHOTO has not been allocated yet !!
    IF(ALLOCATED(PHOTO)) THEN
      DEALLOCATE(PHOTO)
    END IF
    ALLOCATE(PHOTO(NPHOT))
    
    !! ensure that HET has not been allocated yet !!
    IF(ALLOCATED(HET)) THEN
      DEALLOCATE(HET)
    END IF
    ALLOCATE(HET(1))

    !! set global initial values for concentrations (ppm)
    conc(78) = 372.9    !! CO2  !halo: 107      alt:  71    mit DMS + 2
    conc(79) = 500.0    !! H2   !halo: 108      alt:  72
    conc(80) = 20.9E+4  !! O2   !halo: 109      alt:  73
    conc(81) = 78.1E+4  !! N2   !halo: 110      alt:  74
    conc(83) = 1.8      !! CH4  !halo: 112      alt:  76


    int_control = 0
    IF(PRESENT(i_control)) int_control  = i_control
    
    real_control = 0.0d0
    IF(PRESENT(r_control)) real_control = r_control

    IF(analysis) THEN
      OPEN(unit=123,file='integrator_analysis.txt',status='replace', &
           action='write', iostat=i)
      WRITE(123,*) 'Information for integrator analysis:'
      WRITE(123,*) 'processor id'
      WRITE(123,*) 'grid indices i,j,k'
      WRITE(123,*) 'runtime for integrator call'
      WRITE(123,*) 'no. of function evaluations'
      WRITE(123,*) 'no. of Jacobian computations'
      WRITE(123,*) 'no. of integration steps'
      WRITE(123,*) 'no. of accepted steps'
      WRITE(123,*) 'no. of rejected steps'
      WRITE(123,*) 'no. of LU decompositions'
      WRITE(123,*) 'no. of forward/backward substitutions'
      WRITE(123,*) 'no. of singular matrix decompositions'
      WRITE(123,*) 'simulation time at exit from integrator'
      WRITE(123,*) 'last accepted step size'
      WRITE(123,*) 'last predicted step size'
      WRITE(123,*) 'first accepted step size'
    END IF


  END SUBROUTINE Kpp_Init

  SUBROUTINE Kpp_Step(T_start, T_end, TEMP_new, M_new, PHOTO_new, HET_new, conc, &
                      int_status, real_status)
                      
    IMPLICIT NONE

    REAL(kind=dp), INTENT(in) :: T_start, T_end, TEMP_new, M_new
    REAL(kind=dp), DIMENSION(NPHOT), INTENT(in)    :: PHOTO_new
    REAL(kind=dp), DIMENSION(1), INTENT(in)    :: HET_new
    REAL(kind=dp), DIMENSION(NVAR) , INTENT(inout) :: conc
    INTEGER , DIMENSION(8), INTENT(out), OPTIONAL :: int_status
    REAL(kind=dp), DIMENSION(4), INTENT(out), OPTIONAL :: real_status
    INTEGER :: i

    CALL Shuffle_user2kpp(conc, VAR)    

    TIME = T_start        

    TEMP = TEMP_new

    DO i = 1,NPHOT
      PHOTO(i) = PHOTO_new(i)
    END DO
    
    DO i=1,1
      HET(i) = HET_new(i)
    END DO

    M = M_new
    CFACTOR = M_new * 1.0e-6 !! units conversion factor
    
    VAR = VAR * CFACTOR !! ppm -> molec / cm^3

    CALL Update_RCONST()

    CALL INTEGRATE( TIN = T_start, TOUT = T_end, &
                    ISTATUS_U = i_state, RSTATUS_U = r_state, &
                    ICNTRL_U = int_control, RCNTRL_U = real_control )
                    
    VAR = VAR / CFACTOR !! molec / cm^3 -> ppm

    CALL Shuffle_kpp2user(VAR, conc)

    IF(PRESENT(int_status)) THEN
      DO i = 1,8
        int_status(i) = i_state(i)
      END DO
    END IF

    IF(PRESENT(real_status)) THEN
      DO i = 1,4
        real_status(i) = r_state(i)
      END DO
    END IF

  END SUBROUTINE Kpp_Step

END MODULE kpp_Main

! End of MAIN function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


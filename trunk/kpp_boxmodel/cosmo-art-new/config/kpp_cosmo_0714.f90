MODULE KPP_ROOT_Main

  USE KPP_ROOT_Model
  USE KPP_ROOT_Global
  USE KPP_ROOT_Initialize
  
  implicit none

  KPP_REAL, dimension(20) :: r_state
  KPP_REAL, dimension(20) :: real_control
  integer, dimension(20) :: i_state
  integer, dimension(20) :: int_control

  CONTAINS

  SUBROUTINE Kpp_Init(n_phot, conc, analysis, i_control, r_control)

    implicit none

    integer, intent(in) :: n_phot
    KPP_REAL, dimension(NVAR), intent(inout) :: conc
    logical, intent(in) :: analysis
    integer, dimension(20), intent(in), optional :: i_control
    KPP_REAL, dimension(20), intent(in), optional :: r_control
    integer :: i
    
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
    if(present(i_control)) int_control  = i_control
    
    real_control = 0.0d0
    if(present(r_control)) real_control = r_control

    if(analysis) then
      open(unit=123,file='integrator_analysis.txt',status='replace', &
           action='write', iostat=i)
      write(123,*) 'Information for integrator analysis:'
      write(123,*) 'processor id'
      write(123,*) 'grid indices i,j,k'
      write(123,*) 'runtime for integrator call'
      write(123,*) 'no. of function evaluations'
      write(123,*) 'no. of Jacobian computations'
      write(123,*) 'no. of integration steps'
      write(123,*) 'no. of accepted steps'
      write(123,*) 'no. of rejected steps'
      write(123,*) 'no. of LU decompositions'
      write(123,*) 'no. of forward/backward substitutions'
      write(123,*) 'no. of singular matrix decompositions'
      write(123,*) 'simulation time at exit from integrator'
      write(123,*) 'last accepted step size'
      write(123,*) 'last predicted step size'
      write(123,*) 'first accepted step size'
    end if


  END SUBROUTINE Kpp_Init

  SUBROUTINE Kpp_Step(T_start, T_end, TEMP_new, M_new, PHOTO_new, HET_new, conc, &
                      int_status, real_status)
                      
    implicit none

    KPP_REAL, intent(in) :: T_start, T_end, TEMP_new, M_new
    KPP_REAL, DIMENSION(NPHOT), intent(in)    :: PHOTO_new
    KPP_REAL, DIMENSION(1), intent(in)    :: HET_new
    KPP_REAL, DIMENSION(NVAR) , intent(inout) :: conc
    INTEGER , DIMENSION(8), intent(out), optional :: int_status
    KPP_REAL, DIMENSION(4), intent(out), optional :: real_status
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

    if(present(int_status)) then
      do i = 1,8
        int_status(i) = i_state(i)
      end do
    end if

    if(present(real_status)) then
      do i = 1,4
        real_status(i) = r_state(i)
      end do
    end if

  END SUBROUTINE Kpp_Step

END MODULE KPP_ROOT_Main


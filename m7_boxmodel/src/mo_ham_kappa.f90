!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename 
!! mo_ham_kappa.f90
!!
!! \brief
!! Defines data for ham_kappa routine in mo_m7
!!
!! \author Declan O'Donnell (MPI-M)
!!
!! \responsible_coder
!! Declan O'Donnell, declan.Odonnell@fmi.fi
!!
!! \revision_history
!!   -# Declan O'Donnell (MPI-Met) - original code (2007)
!!
!! \limitations
!! None
!!
!! \details
!! ham_kappa implements the parameterisation of the hygroscopic
!! growth of aerosols, as measured by the growth factor (gf), as a
!! function of  the hygroscopicity parameter kappa and 
!! the ambient temperature and relative humidity. The relation is:
!!            [    A   ]      (gf**3 -1)
!!      RH*exp[- ----- ] = ----------------
!!            [  Rd*gf ]   gf**3 - (1-kappa)
!! 
!! (eqn 11 of M.D. Petters and S.M. Kreidenweis, ACP 7, 2007, see 
!! bibliographic references below)
!! 
!! The above-quoted equation from the cited Petters & Kreidenweis paper expresses
!! the hygroscopic growth factor (gf) as a function of aerosol dry radius (Rd),
!! temperature (T), relative humidity (rh) and a substance property denoted kappa
!! that encapsulates hygroscopic properties of that substance. This transcendental
!! equation is solved offline for various Rd, T, rh and kappa of atmospheric
!! relevance and the results stored in a lookup table. 
!! The m7 subroutine ham_kappa uses the ambient T and rh, uses the mode count median 
!! dry radius and the volume-weighted average kappa to find the entry point into
!! the GF lookup table. Using the growth factor the wet median radius and water
!! uptake of the mode are calculated.
!!   
!! This module contains relevant constants and a routine to read the lookup table
!!
!! \bibliographic_references
!! M.D. Petters and S.M. Kreidenweis, A single parameter representation of hygroscopic 
!! growth and cloud condensation nucleus activity, ACP 7, 1961-1971, 2007
!!
!! \belongs_to
!!  HAMMOZ
!!
!! \copyright
!! Copyright and licencing conditions are defined in the ECHAM-HAMMOZ
!! licencing agreement to be found at:
!! https://redmine.hammoz.ethz.ch/projects/hammoz/wiki/1_Licencing_conditions
!! The ECHAM-HAMMOZ software is provided "as is" and without warranty of any kind.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE mo_ham_kappa

  !--- inherited types, functions and data
  USE mo_kind,           ONLY: dp
  USE mo_constants,      ONLY: argas
  USE mo_netcdf
  
  IMPLICIT NONE

  !--- public member functions
  PUBLIC :: start_kappa, term_kappa
  
  !--- module parameters 
  REAL(dp), PARAMETER :: sigma_sa = 0.072_dp    ! Surface tension of pure water [J M-2] 

  !--- public module data 
  !    array dimensions
  REAL(dp), ALLOCATABLE, PUBLIC :: Rd(:)
  REAL(dp), ALLOCATABLE, PUBLIC :: T(:)
  REAL(dp), ALLOCATABLE, PUBLIC :: rh(:)
  REAL(dp), ALLOCATABLE, PUBLIC :: kappa(:)

  !    Growth Factor array
  REAL(dp), ALLOCATABLE, PUBLIC :: gf(:,:,:,:)

  !    max and min values in each coordinate
  REAL(dp), PUBLIC :: Rd_min, Rd_max, ln_Rd_min, ln_Rd_max
  REAL(dp), PUBLIC :: T_min, T_max
  REAL(dp), PUBLIC :: rh_min, rh_max
  REAL(dp), PUBLIC :: kappa_min, kappa_max

  !    number of increments on each coordinate axis
  INTEGER, PUBLIC :: N_Rd
  INTEGER, PUBLIC :: N_T
  INTEGER, PUBLIC :: N_rh
  INTEGER, PUBLIC :: N_kappa

  !--- local data
  ! 
  !--- parameters
  !
  !    file name for lookup table
  CHARACTER(LEN=32), PARAMETER, PRIVATE :: lut_fn = "lut_kappa.nc"

  !    dimension names in lookup table
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cdimn_Rd = "DryRadius"
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cdimn_T  = 'Temperature'
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cdimn_rh = "RelativeHumidity"
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cdimn_k = "kappa"

  !    variable names in lookup table
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cvarn_Rd = "DryRadius"
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cvarn_T = "Temperature"
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cvarn_rh = "RelativeHumidity"
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cvarn_k = "kappa"
  CHARACTER(LEN=16), PARAMETER, PRIVATE :: cvarn_gf= "GF"

  !--- member functions
CONTAINS
  
  SUBROUTINE start_kappa

    ! start_kappa reads the growth factor lookup table
    ! start_kappa is called from init_subm in mo_submodel_interface

    USE mo_read_netcdf77, ONLY: read_var_nf77_1d, read_var_nf77_4d

    IMPLICIT NONE
    
    !---local data
    INTEGER :: zncid                 ! netcdf file identifier
    INTEGER :: zdimid_Rd, zdimid_T, &! netcdf dimension identifiers
               zdimid_rh, zdimid_k
    INTEGER :: idum

    !--- executable procedure

       !---open netcdf file
       CALL nf_check(nf__open(TRIM(lut_fn), nf_nowrite, chunksize, zncid), fname=TRIM(lut_fn))

       !---get dimensions
       !   dry radius
       CALL IO_INQ_DIMID(zncid, TRIM(cdimn_Rd), zdimid_Rd)
       CALL IO_INQ_DIMLEN(zncid, zdimid_Rd, N_Rd)

       !    temperature
       CALL IO_INQ_DIMID(zncid, TRIM(cdimn_T), zdimid_T)
       CALL IO_INQ_DIMLEN(zncid, zdimid_T, N_T)

       !    relative humidity
       CALL IO_INQ_DIMID(zncid, TRIM(cdimn_rh), zdimid_rh)
       CALL IO_INQ_DIMLEN(zncid, zdimid_rh, N_rh)

       !    kappa
       CALL IO_INQ_DIMID(zncid, TRIM(cdimn_k), zdimid_k)
       CALL IO_INQ_DIMLEN(zncid, zdimid_k, N_kappa)

    !---allocate arrays
    !   1.coordinate variables
    !   In runtime, we use only the minimum and maximum value of each
    !   coordinate variable. So we read these from the lookup file 
    !   on the I/O processor, from which we find Tmin, Tmax, etc. and 
    !   then we broadcast the min/max values over the processors.
    
       ALLOCATE(Rd(N_Rd))
       ALLOCATE(T(N_T))
       ALLOCATE(rh(N_rh))
       ALLOCATE(kappa(N_kappa))

    !---GF lookup. This is needed on every processor
       ALLOCATE(gf(N_Rd, N_T, N_rh, N_kappa))

       !---coordinate variables
       CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_Rd), TRIM(cvarn_Rd), Rd, idum)
       CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_T), TRIM(cvarn_T), T, idum)
       CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_rh), TRIM(cvarn_rh), rh, idum)
       CALL read_var_nf77_1d(lut_fn, TRIM(cdimn_k), TRIM(cvarn_k), kappa, idum)

       !---GF lookup table
       CALL read_var_nf77_4d(lut_fn, TRIM(cdimn_Rd), TRIM(cdimn_T), TRIM(cdimn_rh), &
                                     TRIM(cdimn_k), TRIM(cvarn_gf), gf, idum)

    !---GF lookup. This is needed on every processor
       CALL nf_check(nf_close(zncid), fname=TRIM(lut_fn))

       !---get min and max values of the coordinate variables
       Rd_min = Rd(1)
       Rd_max = Rd(N_Rd)
       ln_Rd_min = LOG(Rd_min)
       ln_Rd_max = LOG(Rd_max)
       T_min = T(1)
       T_max = T(N_T)
       rh_min = rh(1)
       rh_max = rh(N_rh)
       kappa_min = kappa(1)
       kappa_max = kappa(N_kappa)

       WRITE(0,*) '----------------------------------------------------------'
       WRITE(0,*) '----------------------------------------------------------'
       WRITE(0,*) '--- Min. radius: ', Rd_min
       WRITE(0,*) '--- Max. radius: ', Rd_max
       WRITE(0,*) '--- Min. temperature: ', T_min
       WRITE(0,*) '--- Max. temperature: ', T_max
       WRITE(0,*) '--- Min. RH : ', rh_min
       WRITE(0,*) '--- Max. RH : ', rh_max
       WRITE(0,*) '--- Min. hygroscopicity: ', kappa_min
       WRITE(0,*) '--- Max. hygroscopicity: ', kappa_max
       WRITE(0,*) '----------------------------------------------------------'
       WRITE(0,*) '----------------------------------------------------------'


    !---Release memory holding the coordinate variables

       DEALLOCATE(Rd)
       DEALLOCATE(T)
       DEALLOCATE(rh)
       DEALLOCATE(kappa)

  END SUBROUTINE start_kappa


  SUBROUTINE term_kappa

    ! term_kappa frees memory allocated for the growth fator lookup table
    ! term_kappa is called from free_submodel_memory in call_submodels

    IMPLICIT NONE

    ! --- executable procedure

    DEALLOCATE(gf)

  END SUBROUTINE term_kappa

END MODULE mo_ham_kappa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename 
!! mo_ham_species.f90
!!
!! \brief
!! mo_ham_species assigns identities and properties to species in the HAM model
!!
!! \author Declan O'Donnell (MPI-Met)
!!
!! \responsible_coder
!! Declan O'Donnell, declan.Odonnell@fmi.fi
!!
!! \revision_history
!!   -# Declan O'Donnell (MPI-Met) - original code (2008)
!!   -# K. Zhang (MPI-Met) - seperate species list for ham; new_species (2009-07) 
!!
!! \limitations
!! None
!!
!! \details
!! None
!!
!! \bibliographic_references
!! None
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

MODULE mo_ham_species
  
  !--- inherited types, data and functions
  USE mo_kind,       ONLY: dp
  USE mo_species,    ONLY: t_species 
  

  IMPLICIT NONE

  !--- public member functions
  
  PUBLIC :: ham_species 

  !--- public module data

  
  !   Maximum number of species in the model
  INTEGER, PARAMETER, PUBLIC :: nmaxspec = 100 

  !   basic HAM model, gas phase compounds
  INTEGER, PUBLIC :: id_dms,                  & ! Dimethyl sulphide
                     id_so2,                  & ! Sulphur dioxide
                     id_so4g,                 & ! Sulphate (gas)
                     id_oh,                   & ! Hydroxyl radical
                     id_h2o2,                 & ! Hydrogen Peroxide
                     id_o3,                   & ! Ozone
                     id_no2                     ! Nitrogen dioxide

  !   basic HAM model, aerosol phase compounds
  INTEGER, PUBLIC :: id_so4,                  & ! Sulphate (aerosol)
                     id_bc,                   & ! Black carbon
                     id_oc,                   & ! Organic carbon 
                     id_ss,                   & ! Sea Salt
                     id_du,                   & ! Dust
                     id_wat                     ! Aerosol water

  ! Number of model species
  INTEGER, PUBLIC :: ham_nspec      ! number of species in HAM
  INTEGER, PUBLIC :: ham_naerospec  ! number of aerosol species in HAM

  ! Model species. These arrays are overdimensioned: the sum of defined species 
  ! (gas plus aerosol) shall not exceed nmaxspec
  TYPE(t_species), PUBLIC, ALLOCATABLE, TARGET :: ham_aerospec(:)
  
  
  
  CONTAINS




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! Register ham gas and aerosol species (except for SOA) 
!!
!! @author 
!! D. O'Dannel (MPI-Met) 
!! K. Zhang    (MPI-Met) 
!!
!! $Id: 1423$
!!
!! @par Revision History
!! D. O'Dannel (MPI-Met) - original version - (2008-??) 
!! K. Zhang    (MPI-Met) - seperate species list for ham - (2009-07) 
!!
!! @par This subroutine is called by
!! to-be-filled
!!
!! @par Externals:
!! <ol>
!! <li>new_species 
!! </ol>
!!
!! @par Notes 
!!  
!! @par Responsible coder
!! kai.zhang@zmaw.de
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE ham_species

  !---inherited types, data and function

  USE mo_tracdef,         ONLY: GAS, AEROSOL, GAS_OR_AEROSOL
  USE mo_species,         ONLY: new_species

  IMPLICIT NONE

     CALL new_species(nphase      = GAS,                 &
                      longname    = 'Dimethyl sulphide', &
                      shortname   = 'DMS',               &
                      units       = 'kg kg-1',           &
!gf                      mw          = 62.019_dp,           &
                      mw          = 62.1345_dp,          &
                      henry       = (/ 0.56_dp, 0._dp /),&
                      idx         = id_dms               )

     !--------- 2. Sulphur Dioxide 
     !

     CALL new_species(nphase      = GAS,                 &
                      longname    = 'Sulphur Dioxide',   &
                      shortname   = 'SO2',               &
                      units       = 'kg kg-1',           &
!gf                      mw          = 63.692_dp,           &
                      mw          = 64.0643_dp,           &
                      henry       = (/ 1.23_dp, 3020._dp /), & 
                      idx         = id_so2               )


     !--------- 4. Hydroxyl Radical 
     !             

     CALL new_species(nphase      = GAS,                 &
                      longname    = 'Hydroxyl radical',  &
                      shortname   = 'OH',                &
                      units       = 'VMR',               &
                      mw          = 17.003_dp,           &
                      henry       = (/ 25._dp, 0._dp /), &
                      idx         = id_oh                )
  
     !--------- 5. Hydrogen Peroxide
     !

     CALL new_species(nphase      = GAS,                 &
                      longname    = 'Hydrogen peroxide', &
                      shortname   = 'H2O2',              &
                      units       = 'VMR',               &
                      mw          = 34.005_dp,           &
                      henry       = (/ 7.45E4_dp, 0._dp /), &
                      idx         = id_h2o2              )

     !--------- 6. Ozone
     !

     CALL new_species(nphase      = GAS,                 &
                      longname    = 'Ozone',             &
                      shortname   = 'O3',                &
                      units       = 'VMR',               &
                      mw          = 48.0_dp,             &
                      henry       = (/ 1.13E-2_dp, 0._dp /), &
                      idx         = id_o3                )

     !--------- 7. Nitrogen Dioxide
     !

     CALL new_species(nphase      = GAS,                 &
                      longname    = 'Nitrogen dioxide',  &
                      shortname   = 'NO2',               &
                      units       = 'VMR',               &
                      mw          = 46.006_dp,           &
                      henry       = (/ 1.0E-2_dp, 0._dp /), &
                      idx         = id_no2               )

     !--------- end of basic model gas phase species

     !--------- basic model aerosol species

     !--------- 3. Gas phase sulphate
     !  sulphate can be present in both the gas-phase and the aerosol phase
     !  density here is that of H2SO4 

     CALL new_species(nphase      = GAS_OR_AEROSOL,      &
                      longname    = 'Sulphate',          &
                      shortname   = 'SO4',               &
                      units       = 'kg kg-1',           &
!gf                      mw          = 95.952_dp,           &
                      mw          = 96.0631_dp,           &
                      henry       = (/ 0._dp, 0._dp /),  &
                      density      = 1841._dp,           &
                      lelectrolyte = .TRUE.,             &
                      kappa        = 0.60_dp,            &
                      idx          = id_so4g             )

     ! equate "aerosol" species id with "gas" species id for SO4
     id_so4 = id_so4g

     !--------- 9. Black Carbon
     !

     CALL new_species(nphase       = AEROSOL,             &
                      longname     = 'Black carbon',      &
                      shortname    = 'BC',                &
                      units        = 'kg kg-1',           &
                      mw           = 12.010_dp,           &
                      density      = 2000._dp,            & 
                      kappa        = 0._dp,               &
                      idx          = id_bc                )


     !--------- 10. Organic Carbon (Primary organic aerosol)
     !

     CALL new_species(nphase       = AEROSOL,             &
                      longname     = 'Organic carbon',    &
                      shortname    = 'OC',                &
                      units        = 'kg kg-1',           &
                      mw           = 180._dp,             &
                      density      = 2000._dp,            &
                      kappa        = 0.06_dp,             & 
                      idx          = id_oc                )


     !--------- 11. Sea salt
     !

     CALL new_species(nphase       = AEROSOL,             &
                      longname     = 'Sea salt',          &
                      shortname    = 'SS',                &
                      units        = 'kg kg-1',           &
                      mw           = 58.443_dp,           &
                      density      = 2165._dp,            &
                      lelectrolyte = .TRUE.,              &
                      kappa        = 1.12_dp,             &
                      idx          = id_ss                )


     !--------- 12. Mineral Dust
     !

     CALL new_species(nphase       = AEROSOL,             &
                      longname     = 'Dust',              &
                      shortname    = 'DU',                &
                      units        = 'kg kg-1',           &
                      mw           = 250._dp,             &
                      density      = 2650._dp,            &
                      kappa        = 0._dp,               &
                      idx          = id_du                )


     !--------- 13. Water (includes only water on aerosols, not water vapour, cloud water or cloud ice)
     !                    

     CALL new_species(nphase       = AEROSOL,             &
                      longname     = 'Aerosol water',     &
                      shortname    = 'WAT',               &
                      units        = 'kg kg-1',           &
                      mw           = 18.0_dp,             &
                      density      = 1000._dp,            &
                      kappa        = 0._dp,               &
                      idx          = id_wat               )

  END SUBROUTINE ham_species

END MODULE mo_ham_species


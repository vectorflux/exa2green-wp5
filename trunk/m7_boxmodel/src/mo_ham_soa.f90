!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename 
!! mo_ham_soa.f90
!!
!! \brief
!! This module contains HAM routines related to the handling of secondary organic aerosols.
!!
!! \author  Declan O'Donnell (MPI-M)
!!
!! \responsible_coder
!! Declan O'Donnell, declan.Odonnell@fmi.fi 
!!
!! \revision_history
!!   -# Declan O'Donnell (MPI-Met) - original code (YYYY)
!!   -# Kai Zhang (MPI-Met) - rewrite species registeration part, change longname
!!                            and shortname to avoid duplicate longnames in species list (2009-08)
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

MODULE mo_ham_soa

  !--- inherited types, data and functions
  USE mo_kind,         ONLY: dp       
  USE mo_ham,          ONLY: nsoa, new_aerocomp
  USE mo_ham_species,  ONLY: id_oc
  USE mo_species,      ONLY: t_species, new_species
  USE mo_tracdef,      ONLY: GAS, AEROSOL, GAS_OR_AEROSOL

  IMPLICIT NONE  

  !---public member functions
  
  PUBLIC :: soa_species
  PUBLIC :: start_soa_aerosols

  !---public data types 

  TYPE, PUBLIC :: t_soa_prop                           ! SOA properties
     LOGICAL  :: lvolatile                             ! Volatile species T/F
     REAL(dp) :: Kp                                    ! 2-product model parameter: 
                                                       ! partitioning coefficient 
     REAL(dp) :: tref                                  ! 2-product model parameter: 
                                                       ! temperature at which Kp was derived
     REAL(dp) :: dH                                    ! 2-product model parameter:
                                                       ! Enthalpy of vaporisation
     INTEGER  :: spid_tot                              ! Species id, prognostic species (gas+aerosol total mass)
     INTEGER  :: spid_soa                              ! Species id, diagnostic species (gas or aerosol)
  END TYPE t_soa_prop                                  

  !---public module data

  !---maximum number of SOA species
  INTEGER, PARAMETER, PUBLIC :: nmaxsoa = 7

  !---species identities for SOA precursors, gases and aerosols  ### now in mo_soa_species
  !   soa precursors
  INTEGER, PUBLIC :: id_mterp, id_isop, id_tol, id_xyl, id_benz

  INTEGER, PUBLIC :: id_no3, id_ho2                    ! NO3 not in basic HAM model

  !---2-product model properties
  TYPE(t_soa_prop), PUBLIC, TARGET :: soaprop(nmaxsoa)         

  INTEGER, PUBLIC :: nnvol                 ! number of non-volatile species

  !---model functional switches
  LOGICAL, PUBLIC  :: lso4_in_m0 = .FALSE.                ! include sulphate in SOA absorbing mass?

  !-----------------------------------------------------------------------------------------------

  CONTAINS
  !-----------------------------------------------------------------------------------------------

    SUBROUTINE soa_species

      USE mo_ham,           ONLY: nsoalumping
      USE mo_tracdef,       ONLY: GAS, AEROSOL, GAS_OR_AEROSOL
      
      IMPLICIT NONE

      !---local data:
      INTEGER :: i
      !---executable procedure


     !>> dod
     ! semi-volatile species are handled in a special way. Such species may exist both in the
     ! gas and aerosol phases, and in many different aerosol size modes. Partitioning between
     ! gas and aerosol is done with an equilibrium model. This has the consequence that from the
     ! point of view of tracer transport, we do not actually need separate aerosol and gas phase
     ! tracers: since both gas and aerosol are transported in the same way, we can simply transport
     ! the total mass (gas+aerosol) and then diagnose the gas and aerosol masses using the 
     ! equilibrium model. This cuts the tracer transport requirements by a factor of 5 or so. 
     ! For purposes of calculating radiative effects, wet deposition,
     ! etc., it is convenient to also implement the separated gas and aerosols as tracers.
  
     ! First the prognostic species (total). Define it as a gas, otherwise HAM will
     ! allocate many tracers.
     CALL new_species(nphase      = GAS,                          &
                      longname    = 'Monoterpene SOA Total 1',    &
                      shortname   = 'SOA_MT_T1',                  &
                      units       = 'kg kg-1',                    &
                      mw          = 186._dp,                      &
                      idx         = soaprop(1)%spid_tot           ) 


     CALL new_species(nphase      = GAS,                          &
                      longname    = 'Monoterpene SOA Total 2',    &
                      shortname   = 'SOA_MT_T2',                  &
                      units       = 'kg kg-1',                    &
!gf                      mw          = 186._dp,                      &
                      mw          = 168._dp,                      &
                      idx         = soaprop(2)%spid_tot           ) 

     
     ! now the gases and aerosols...
     CALL new_species(nphase      = GAS_OR_AEROSOL,               &
                      longname    = 'Monoterpene SOA 1',          &
                      shortname   = 'SOA_MT1',                    &
                      units       = 'kg kg-1',                    &
                      mw          = 186._dp,                      & 
                      density     = 1320._dp,                     &
                      lwatsol     = .TRUE.,                       &  
!>>gf see #105
!gf                      kappa       = 0.1_dp,                       & 
                      kappa       = 0.037_dp,                     & !Petters and Kreidenweis (2007)
!<<gf
                      idx         = soaprop(1)%spid_soa           ) 


     CALL new_species(nphase      = GAS_OR_AEROSOL,               &
                      longname    = 'Monoterpene SOA 2',          &
                      shortname   = 'SOA_MT2',                    &
                      units       = 'kg kg-1',                    &
!gf                      mw          = 186._dp,                      &     
                      mw          = 168._dp,                      &     
                      density     = 1320._dp,                     &
                      lwatsol     = .TRUE.,                       &  
!>>gf see #105
!gf                      kappa       = 0.1_dp,                       &
                      kappa       = 0.037_dp,                     & !Petters and Kreidenweis (2007) 
!<<gf
                      idx         = soaprop(2)%spid_soa           ) 


     ! End of monoterpene definition

     ! Isoprene

     ! Prognostic species (total mass)
     CALL new_species(nphase      = GAS,                            &
                      longname    = 'Isoprene SOA Total 1',         &
                      shortname   = 'SOA_IS_T1',                    &
                      units       = 'kg kg-1',                      &
                      mw          = 125._dp,                        &
                      idx         = soaprop(3)%spid_tot             ) 

     CALL new_species(nphase      = GAS,                            &
                      longname    = 'Isoprene SOA Total 2',         &
                      shortname   = 'SOA_IS_T2',                    &
                      units       = 'kg kg-1',                      &
                      mw          = 125._dp,                        &
                      idx         = soaprop(4)%spid_tot             ) 
				      
     ! Diagnostic species. 
     CALL new_species(nphase      = GAS_OR_AEROSOL,                 &
                      longname    = 'Isoprene SOA 1',               &
                      shortname   = 'SOA_IS1',                      &
                      units       = 'kg kg-1',                      &
                      mw          = 125._dp,                        &
                      density     = 1320._dp,                       &  
!>>gf see #105
!gf                      kappa       = 0.1_dp,                         & 
                      kappa       = 0.037_dp,                       & !Petters and Kreidenweis (2007) 
!<<gf
                      idx         = soaprop(3)%spid_soa             ) 

     CALL new_species(nphase      = GAS_OR_AEROSOL,                 &
                      longname    = 'Isoprene SOA 2',               &
                      shortname   = 'SOA_IS2',                      &
                      units       = 'kg kg-1',                      &
                      mw          = 125._dp,                        & 
                      density     = 1320._dp,                       &  
!>>gf see #105
!gf                      kappa       = 0.1_dp,                         & 
                      kappa       = 0.037_dp,                       & !Petters and Kreidenweis (2007) 
!<<gf
                      idx         = soaprop(4)%spid_soa             ) 

     ! End isoprene definitons

     !---create anthropogenic gas phase SOA species according to lumping parameter
     ! 
     SELECT CASE(nsoalumping)
     CASE(0)                              ! no lumping, all anthropogenics distinct 

        CALL new_species(nphase      = AEROSOL,                      &
                         longname    = 'Toluene SOA',                &
                         shortname   = 'SOA_TOL',                    &
                         units       = 'kg kg-1',                    &
                         mw          = 124._dp,                      & 
                         density     = 1450._dp,                     &
                         lwatsol     = .TRUE.,                       &  
!>>gf see #105
!gf                         kappa       = 0.1_dp,                       & 
                         kappa       = 0.037_dp,                     & !Petters and Kreidenweis (2007)
!<<gf
                         idx         = soaprop(5)%spid_soa               ) 


        CALL new_species(nphase      = AEROSOL,                      &
                         longname    = 'Xylene SOA',                 &
                         shortname   = 'SOA_XYL',                    &
                         units       = 'kg kg-1',                    &
                         mw          = 138._dp,                      & 
                         density     = 1330._dp,                     &
                         lwatsol     = .TRUE.,                       &  
!>>gf see #105
!gf                         kappa       = 0.1_dp,                       & 
                         kappa       = 0.037_dp,                     & !Petters and Kreidenweis (2007)
!<<gf
                         idx         = soaprop(6)%spid_soa              ) 

        CALL new_species(nphase      = AEROSOL,                      &
                         longname    = 'Benzene SOA',                &
                         shortname   = 'SOA_BENZ',                   &
                         units       = 'kg kg-1',                    &
                         mw          = 98._dp,                       & 
                         density     = 1450._dp,                     &
                         lwatsol     = .TRUE.,                       &  
!>>gf see #105
!gf                         kappa       = 0.1_dp,                       & 
                         kappa       = 0.037_dp,                     & !Petters and Kreidenweis (2007)
!<<gf
                         idx         = soaprop(7)%spid_soa              ) 

     CASE(1)                               ! lump all anthropogenics into one distinct SOA species

        CALL new_species(nphase      = AEROSOL,                      &
                         longname    = 'Anthropogenic SOA',          &
                         shortname   = 'ASOA',                       &
                         units       = 'kg kg-1',                    &
                         mw          = 124._dp,                      & 
                         density     = 1450._dp,                     &
                         lwatsol     = .TRUE.,                       &  
!>>gf see #105
!gf                         kappa       = 0.1_dp,                       & 
                         kappa       = 0.037_dp,                     & !Petters and Kreidenweis (2007)
!<<gf
                         idx         = soaprop(5)%spid_soa              ) 
        
     CASE(2)                              ! lump all anthropogenic SOA together with primary OC

        !! soaprop(5)%spid_soa = id_oc

     END SELECT

     !---SOA 2-product properties

     !---biogenics
     !   monoterpene SOA
     soaprop(1)%lvolatile = .TRUE.
     soaprop(2)%lvolatile = .TRUE.
     
     !   isoprene SOA
     soaprop(3)%lvolatile = .TRUE.
     soaprop(4)%lvolatile = .TRUE.

     !---anthropogenics according to lumping parameter
     SELECT CASE(nsoalumping) 
     CASE(0)                              ! no lumping, all anthropogenics distinct 
        !   toluene SOA
        soaprop(5)%lvolatile = .FALSE.
        soaprop(5)%spid_tot = -1
        
        !   xylene SOA
        soaprop(6)%lvolatile = .FALSE.
        soaprop(6)%spid_tot = -1

        !  benzene SOA
        soaprop(7)%lvolatile = .FALSE.
        soaprop(7)%spid_tot = -1

        nsoa = 7

     CASE(1)                            ! lump all anthropogenics into one distinct SOA species

        soaprop(5)%lvolatile = .FALSE.
        soaprop(5)%spid_tot = -1
        
        nsoa = 5

     CASE(2)
        nsoa = 4
     END SELECT

     !---set species id for unused array elements to 'undefined'
     IF (nsoa < nmaxsoa) THEN
        soaprop(nsoa+1:nmaxsoa)%spid_tot = -1
        soaprop(nsoa+1:nmaxsoa)%spid_soa = -1
     END IF

     !---count the number of non-volatile species (needed by soa2prod and soa_part
     !   see mo_ham_soa_processes).
     nnvol = 0
     DO i=1,nsoa
        IF (.NOT. soaprop(i)%lvolatile) nnvol = nnvol + 1
     END DO
     
     IF (nsoalumping == 2) nnvol = 1

   END SUBROUTINE soa_species
   
    !-----------------------------------------------------------------------------------------------

    SUBROUTINE start_soa_aerosols(nmod, lsoainmode)

      USE mo_ham,          ONLY: new_aerocomp
      USE mo_species,      ONLY: speclist

      IMPLICIT NONE

      INTEGER, INTENT(in)    :: nmod
      LOGICAL, INTENT(in)    :: lsoainmode(nmod)

      !---local data:
      INTEGER :: jn, jm, ispec 

      !---executable procedure

      DO jn = 1,nmod
         IF (lsoainmode(jn)) THEN

            DO jm=1,nsoa

               ispec = soaprop(jm)%spid_soa

               !---allocate memory for the aerosol phase tracers
               IF (.NOT. ALLOCATED(speclist(ispec)%iaerocomp) ) THEN
                 ALLOCATE(speclist(ispec)%iaerocomp(nmod))
                 speclist(ispec)%iaerocomp(:) = 0
               END IF

               speclist(ispec)%iaerocomp(jn) = new_aerocomp(jn, ispec)
            END DO
         END IF
      END DO

    END SUBROUTINE start_soa_aerosols

END MODULE mo_ham_soa

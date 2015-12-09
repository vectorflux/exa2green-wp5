!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename
!! mo_ham_init.f90
!!
!! \brief
!! Initialisation routines for HAM aerosol model
!!
!! \author Martin Schultz (FZ Juelich)
!!
!! \responsible_coder
!! Martin Schultz, m.schultz@fz-juelich.de
!!
!! \revision_history
!!   -# M. Schultz (FZ Juelich) - original code (2009-09-22)
!!   -# D. O'Donnell, ETH-Z  - added SOA support (2010-04-22)
!!
!! \limitations
!! None
!!
!! \details
!! This module contains the general HAM initialisation routine which is called 
!! from submodel_initialise.
!! It first calls setham to initialize ham module variables and read the hamctl namelist.
!! Then other namelists are read and the M7 module is started. Next, the HAM species are
!! defined and the aerosol modes are populated.
!! Adopted from former init_ham routine. Contributors to
!! the code from which the present routines were derived include:
!! P. Stier, D. O'Donnell, K. Zhang
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

MODULE mo_ham_init

  IMPLICIT NONE

  PRIVATE

  SAVE

  PUBLIC  :: start_ham,            &
             ham_initialize,       &
             ham_init_memory,      &
             ham_free_memory

! Variable declarations


  CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> 
!! start_ham: high-level initialisation routine; interface for HAM aerosol module
!! initialisation including species definition
!! 
!! @author see module info 
!!
!! $Id: 1423$
!!
!! @par Revision History
!! see module info 
!!
!! @par This subroutine is called by
!! init_submodels
!!
!! @par Responsible coder
!! m.schultz@fz-juelich.de
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE start_ham
  
    USE mo_ham,             ONLY : setham,                     &
                                   nmodes=>nmod,               &
                                   lsoa, m7mode
    USE mo_ham_m7ctl,       ONLY : sethamM7, nmod, nsol,       &
                                   m7_naerospec, m7_initialize
    USE mo_ham_m7_species,  ONLY : map_m7_species
    USE mo_ham_species,     ONLY : ham_species
    USE mo_ham_soa,         ONLY : soa_species, start_soa_aerosols

    !-- local variables
    LOGICAL    :: lsoainmode(nmod)    ! copy from m7mode(:)%lsoainmode for code structure reasons
    INTEGER    :: jm
  
    ! -- 1. set default values and read hamctl namelist
    nmodes = nmod       !! make generic ham routines aware of mode count
       
       CALL sethamm7
       CALL setham(nmod)

       ! -- initialize M7 scheme
       CALL m7_initialize
  
    ! -- 3. define aerosol species
       CALL ham_species
       IF (lsoa) CALL soa_species
       
       ! -- map general species list onto M7 condensed list
       CALL map_m7_species
  
       ! -- 4. generate mode x species matrix
       CALL ham_define_modes(nmod, nsol, m7_naerospec)
         IF (lsoa) THEN
          DO jm=1,nmod
             lsoainmode(jm) = m7mode(jm)%lsoainmode
          END DO
          CALL start_soa_aerosols(nmod, lsoainmode)
       END IF

  END SUBROUTINE start_ham

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! ham_define_tracer: create ECHAM tracers based on HAM species
!!
!! @author see module info
!!
!! $Id: 1423$
!!
!! @par Revision History
!! see module info
!!
!! @par This subroutine is called by
!! init_submodels
!!
!! @par Externals:
!! <ol>
!! <li>none
!! </ol>
!!
!! @par Notes
!! ### ToDo: complete allocation of burden diagnostics
!!
!! @par Responsible coder
!! m.schultz@fz-juelich.de
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE ham_define_modes(nmod, nsol, naerospec)

    USE mo_species,                 ONLY: speclist
    USE mo_ham_species,             ONLY: id_so4, &
                                          id_bc,  &
                                          id_oc,  &
                                          id_ss,  &
                                          id_du,  &
                                          id_wat
    USE mo_ham,                     ONLY: aerocomp, aerowater, naerocomp,        &
                                          new_aerocomp
    USE mo_ham_m7ctl,               ONLY: inucs, iaits, iaccs, icoas,          &
                                          iaiti, iacci, icoai,                 &
                                          iso4ns, iso4ks, iso4as, iso4cs,      &
                                          ibcks, ibcas, ibccs, ibcki,          &
                                          iocks, iocas, ioccs, iocki,          &
                                          issas, isscs,                        &
                                          iduas, iducs, iduai, iduci


    INTEGER, INTENT(in)          :: nmod      ! number of aerosol modes
    INTEGER, INTENT(in)          :: nsol      ! number of soluble modes
    INTEGER, INTENT(in)          :: naerospec ! number of aerosol species defined

    INTEGER           :: jn

!gf
!    IF (ALLOCATED(aerocomp)) CALL finish('ham_define_modes', 'aerocomp already allocated!')    ! ###
    IF (ALLOCATED(aerocomp)) THEN
       WRITE(*,*) 'ham_define_modes: aerocomp already allocated!'
       STOP
    ENDIF
!gf

    ALLOCATE(aerocomp(nmod * naerospec))
    ALLOCATE(aerowater(nsol))

    !---1) Aerosol compounds in the basic model (SO4, BC, OC, SS, DU) in applicable modes------
    naerocomp = 0
    
    !---sulphate in soluble modes    
    iso4ns = new_aerocomp(inucs, id_so4)
    iso4ks = new_aerocomp(iaits, id_so4)
    iso4as = new_aerocomp(iaccs, id_so4)
    iso4cs = new_aerocomp(icoas, id_so4)

    IF (.NOT. ALLOCATED(speclist(id_so4)%iaerocomp)) ALLOCATE(speclist(id_so4)%iaerocomp(nmod))
    speclist(id_so4)%iaerocomp(:)     = 0
    speclist(id_so4)%iaerocomp(inucs) = iso4ns
    speclist(id_so4)%iaerocomp(iaits) = iso4ks
    speclist(id_so4)%iaerocomp(iaccs) = iso4as
    speclist(id_so4)%iaerocomp(icoas) = iso4cs
    
    !---black carbon in aitken, accumulation and coarse soluble and aitken insoluble modes
    ibcks = new_aerocomp(iaits, id_bc)
    ibcas = new_aerocomp(iaccs, id_bc)
    ibccs = new_aerocomp(icoas, id_bc)
    ibcki = new_aerocomp(iaiti, id_bc)
    
    IF (.NOT. ALLOCATED(speclist(id_bc)%iaerocomp)) ALLOCATE(speclist(id_bc)%iaerocomp(nmod))
    speclist(id_bc)%iaerocomp(:)     = 0
    speclist(id_bc)%iaerocomp(iaits) = ibcks
    speclist(id_bc)%iaerocomp(iaccs) = ibcas
    speclist(id_bc)%iaerocomp(icoas) = ibccs
    speclist(id_bc)%iaerocomp(iaiti) = ibcki
    !---organic carbon in aitken, accumulation and coarse soluble and aitken insoluble modes
    iocks = new_aerocomp(iaits, id_oc)
    iocas = new_aerocomp(iaccs, id_oc)
    ioccs = new_aerocomp(icoas, id_oc)
    iocki = new_aerocomp(iaiti, id_oc)

    IF (.NOT. ALLOCATED(speclist(id_oc)%iaerocomp)) ALLOCATE(speclist(id_oc)%iaerocomp(nmod))
    speclist(id_oc)%iaerocomp(:)     = 0
    speclist(id_oc)%iaerocomp(iaits) = iocks
    speclist(id_oc)%iaerocomp(iaccs) = iocas
    speclist(id_oc)%iaerocomp(icoas) = ioccs
    speclist(id_oc)%iaerocomp(iaiti) = iocki

    !---sea salt in accumulation and coarse soluble modes
    issas = new_aerocomp(iaccs, id_ss)
    isscs = new_aerocomp(icoas, id_ss)

    IF (.NOT. ALLOCATED(speclist(id_ss)%iaerocomp)) ALLOCATE(speclist(id_ss)%iaerocomp(nmod))
    speclist(id_ss)%iaerocomp(:)     = 0
    speclist(id_ss)%iaerocomp(iaccs) = issas
    speclist(id_ss)%iaerocomp(icoas) = isscs

    !---dust in accumulation and coarse modes
    iduas = new_aerocomp(iaccs, id_du)
    iducs = new_aerocomp(icoas, id_du)
    iduai = new_aerocomp(iacci, id_du)
    iduci = new_aerocomp(icoai, id_du)

    IF (.NOT. ALLOCATED(speclist(id_du)%iaerocomp)) ALLOCATE(speclist(id_du)%iaerocomp(nmod))
    speclist(id_du)%iaerocomp(:)     = 0
    speclist(id_du)%iaerocomp(iaccs) = iduas
    speclist(id_du)%iaerocomp(icoas) = iducs
    speclist(id_du)%iaerocomp(iacci) = iduai
    speclist(id_du)%iaerocomp(icoai) = iduci

    !---2) Aerosol water ----------------------------------------------------------------------
    IF (.NOT. ALLOCATED(speclist(id_wat)%iaerocomp)) ALLOCATE(speclist(id_wat)%iaerocomp(nmod))
    speclist(id_wat)%iaerocomp(:)     = -1
    DO jn = 1,nsol
       aerowater(jn)%iclass = jn
       aerowater(jn)%species => speclist(id_wat)
       aerowater(jn)%spid = id_wat
    END DO
    
  END SUBROUTINE ham_define_modes 

  SUBROUTINE ham_initialize

    ! Purpose:
    ! ---------
    ! Initializes constants and parameters used in the HAM aerosol model.
    ! Performs consistency checks.
    !
    ! Author:
    ! ---------
    ! Philip Stier, MPI                           03/2003
    ! Martin Schultz, FZJ                         09/2009 - renamed to ham_
    !                                                     - cleanup
    !
    ! Interface:
    ! ---------
    ! *ham_initialize*  is called from *init_subm* in mo_submodel_interface
    !                    needs to be called after initialization of the 
    !                    submodel as it makes use of parameters in mo_ham_m7ctl,
    !

    USE mo_ham_m7ctl,       ONLY: nwater 
    USE mo_ham_kappa,       ONLY: start_kappa

    IMPLICIT NONE

    !--- set tracer ids needed for example in wetdep scheme
    !    this must be done *after* definition of MOZ tracers in order to account for HAMMOZ coupling
    
    IF (nwater == 1) CALL start_kappa

  END SUBROUTINE ham_initialize

  ! --- initialisation of memory (output and diagnostic streams etc.)

  SUBROUTINE ham_init_memory

  USE mo_ham_m7ctl,      ONLY: nsnucl, nonucl
  USE mo_ham_nucl,       ONLY: ham_nucl_initialize

  IF (nsnucl+nonucl.gt.0) CALL ham_nucl_initialize

  END SUBROUTINE ham_init_memory

  ! --- release of memory

  SUBROUTINE ham_free_memory

    USE mo_ham_m7ctl,      ONLY: nsnucl, nonucl, nwater
    USE mo_ham_kappa,      ONLY: term_kappa
    USE mo_ham_nucl,       ONLY: ham_nucl_cleanup

    IMPLICIT NONE

    IF (nwater == 1)        CALL term_kappa
    IF (nsnucl+nonucl.gt.0) CALL ham_nucl_cleanup

  END SUBROUTINE ham_free_memory

END MODULE mo_ham_init







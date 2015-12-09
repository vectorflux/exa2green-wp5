!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> 
!! mo_species assigns identities and properties to species.
!! Former ECHAM versions had physical and chemical information stored with tracers which
!! meant duplication of fields when more than one tracer was defined for a given species.
!! Examples are aerosol modes or tagged gas-phase tracers.
!! With mo_species, these species characteristics are defined once and the tracerinfo
!! is trimmed down to the essential quantities that vary with tracer. 
!! Some duplication is unavoidable in order to keep flexibility.
!! 
!! @author 
!! <ol> 
!! <li>Declan O'Donnell (MPI-Met)
!! <li>M. Schultz (FZ-Juelich) 
!! </ol>
!!
!! $Id: 1423$
!!
!! @par Revision History
!! <ol> 
!! <li>Declan O'Donnell (MPI-Met) - original version - (2008-xx-xx) 
!! <li>M. Schultz (FZ-Juelich) - generalisation and only one species list for everything  - (2009-06-xx) 
!! <li>K. Zhang (MPI-Met) - merge Declan's and Martin's version - (2009-08-11) 
!! <li>M. Schultz (FZ-Juelich) - further cleanup (2009-09-21)
!! </ol>
!!
!! @par This module is used by
!! to_be_added
!!  
!! @par Notes
!! 
!!
!! @par Responsible coder
!! m.schultz@fz-juelich.de
!!
!! @par Copyright
!! 2009 by MPI-M
!! This software is provided for non-commercial use only.
!! See the LICENSE and the WARRANTY conditions.
!!
!! @par License
!! The use of ECHAM is hereby granted free of charge for an unlimited time,
!! provided the following rules are accepted and applied:
!! <ol>
!! <li> You may use or modify this code for your own non commercial and non
!! violent purposes.
!! <li> The code may not be re-distributed without the consent of the authors.
!! <li> The copyright notice and statement of authorship must appear in all
!! copies.
!! <li> You accept the warranty conditions (see WARRANTY).
!! <li> In case you intend to use the code commercially, we oblige you to sign
!! an according license agreement with MPI-M.
!! </ol>
!!
!! @par Warranty
!! This code has been tested up to a certain level. Defects and weaknesses,
!! which may be included in the code, do not establish any warranties by the
!! authors.
!! The authors do not make any warranty, express or implied, or assume any
!! liability or responsibility for the use, acquisition or application of this
!! software.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE mo_species

  !--- inherited types, data and functions
  
  USE mo_kind,         ONLY: dp
  USE mo_tracdef,      ONLY: GAS, AEROSOL, GAS_OR_AEROSOL 

  IMPLICIT NONE

  PRIVATE

  !--- public member functions

  PUBLIC :: new_species
  PUBLIC :: query_species

  !--- Parameters and integer declarations
  
  INTEGER, PARAMETER, PUBLIC :: nmaxspec   = 200 ! maximum number of species in the model
  INTEGER, PARAMETER, PUBLIC :: nmaxtrspec =  32 ! maximum number of tracers per species

  !   Number of species 
  INTEGER,            PUBLIC :: nspec            ! number of all  species defined
  INTEGER,            PUBLIC :: naerospec        ! number of species in aerosol phase

  !   Species-Tracer relationship

  INTEGER,            PUBLIC :: spec_idt(nmaxspec, nmaxtrspec)
  INTEGER,            PUBLIC :: spec_ntrac(nmaxspec)

  !   Basic 'species' type common for gas and aerosol.

  TYPE, PUBLIC :: t_species
   
     ! --- basic information --- 
     CHARACTER(LEN=64) :: longname            ! Name of the species
     CHARACTER(LEN=32) :: shortname           ! Shorthand name  
     CHARACTER(LEN=15) :: units               ! Units for 'outside world', usually kg kg-1 
     REAL(dp)          :: moleweight          ! Molecular weight [g mol-1] 
     ! --- physical and chemical parameters ---  
     INTEGER           :: nphase              ! phase of specie 
                                              ! && gas &&
     REAL(dp)          :: henry(2)            ! Henry's law coefficient and activation energy
     REAL(dp)          :: density             ! Density of solid or liquid phase [g cm-3]
     LOGICAL           :: lwatsol             ! Species dissolves in water
                                              ! NBB THIS IS NOT THE SAME AS M7 'soluble', which means
                                              ! that an M7 mode can take up water. This flag means
                                              ! that the species can physically dissolve in water.
                                              ! Should be set = TRUE for weakly soluble substances
                                              ! and FALSE only for e.g. dust, black carbon
     LOGICAL           :: lelectrolyte        ! Species dissociates into ions in aqueous solution
     REAL(dp)          :: kappa               ! Kappa-Koehler coefficient for the species.  
                                              ! See: Petters and Kreidenweis
                                              ! ACP 7, 1961-1971, 2007 
     ! --- accounting ---  
     INTEGER           :: idt                 ! Tracer index for gas-phase tracer
     INTEGER, ALLOCATABLE :: iaerocomp(:)     ! Index to aerocomp array. Dimension nmod.
  END TYPE t_species

  ! derived variables  
  
  TYPE(t_species), TARGET, PUBLIC :: speclist(nmaxspec)     ! aerosol and gas-phase species lists
                                                            ! (analogy to trlist in mo_tracdef)
  INTEGER, PUBLIC                 :: aero_idx(nmaxspec)     ! indices of aerosol species
  ! Note: aero_idx is currently only used to compute reverse index in mo_ham_rad

  CONTAINS
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!> 
!! new_species defines the physical and chemical properties of a chemical species.
!! Normally, each species will also be defined as tracer (new_tracer routine in mo_tracer).
!! The same species can be used in the definition of various tracers (for example
!! different aerosol modes) 
!! 
!! @author see module info 
!!
!! $Id: 1423$
!!
!! @par Revision History
!! see module info 
!!
!! @par This subroutine is called by
!! <ol>
!! <li>ham_define_tracers
!! <li>moz_define_tracers
!! </ol> 
!!
!! @par Externals:
!! <ol>
!! <li>none
!! </ol>
!!
!! @par Notes
!! 
!! @par Responsible coder
!! m.schultz@fz-juelich.de
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  SUBROUTINE new_species(nphase,                                  &
                         longname,                                &
                         shortname,                               &
                         units,                                   &
                         mw,                                      & 
                         henry,                                   &
                         density,                                 & 
                         lwatsol,                                 &
                         lelectrolyte,                            &
                         kappa,                                   &
                         idx)
  
  INTEGER,          INTENT(IN)   :: nphase               ! GAS or AEROSOL

  CHARACTER(LEN=*), INTENT(IN)   :: longname             ! full name (e.g. for diagnostics)
  CHARACTER(LEN=*), INTENT(IN)   :: shortname            ! short name (e.g. variable name in output files) 
  CHARACTER(LEN=*), INTENT(IN)   :: units                ! concentration units, eg 'kg kg-1', 'VMR'
  REAL(dp),         INTENT(IN)   :: mw                   ! Molecular weight (required for conversions) 
  REAL(dp), INTENT(IN), OPTIONAL :: henry(2)             ! Henry coefficient 
  REAL(dp), INTENT(IN), OPTIONAL :: density              ! (aerosol) density
  LOGICAL,  INTENT(IN), OPTIONAL :: lwatsol              ! flag for water soluble (SOA scheme)
  LOGICAL,  INTENT(IN), OPTIONAL :: lelectrolyte         ! flag for dissociation in water
  REAL(dp), INTENT(IN), OPTIONAL :: kappa                ! Kappa-Koehler coefficient
  INTEGER,  INTENT(OUT),OPTIONAL :: idx                  ! index to species list 
  

  !--- Local data
  INTEGER              :: i
  CHARACTER(len=16)    :: cphasenam

  !--- executable procedure
  ! Default return value
  IF (PRESENT(idx))   idx = 0

  ! Check validity of nphase
  SELECT CASE(nphase)
  CASE(GAS)
    cphasenam = 'Gas-phase'
  CASE(AEROSOL)
    cphasenam = 'Aerosol'
  CASE(GAS_OR_AEROSOL)
    cphasenam = 'Gas or Aerosol'
  CASE DEFAULT
!gf    CALL message('new_species', 'Invalid value for nphase. Only GAS or AEROSOL allowed.', level=em_error)
    WRITE(*,*) 'new_species: Invalid value for nphase. Only GAS or AEROSOL allowed.'
    RETURN
  END SELECT

  ! Check if a species of this name has already been defined
  CALL query_species(longname=longname, nphase=nphase, index=i)
  IF (i > 0) THEN
!gf
!     CALL message('new_species', TRIM(cphasenam)//' species '//TRIM(longname)//' already defined.', &
!                  level=em_warn)
!     CALL message('', 'Will use existing properties.', level=em_none)
     WRITE(*,*) 'new_species  --> ', TRIM(cphasenam)//' species --> '//TRIM(longname)//' already defined.'
     STOP
!gf
     IF (PRESENT(idx)) idx = i    ! return index
     RETURN
  END IF
  
  CALL query_species(shortname=shortname, nphase=nphase, index=i)  
  IF (i > 0) THEN
!gf
!     CALL message('new_species', TRIM(cphasenam)//' species '//TRIM(shortname)//' already defined.', &
!                  level=em_warn)
!     CALL message('', 'Will use existing properties.', level=em_none)
     WRITE(*,*) 'new_species  --> ', TRIM(cphasenam)//' species --> '//TRIM(shortname)//' already defined.'
     STOP
!gf
     IF (PRESENT(idx)) idx = i    ! return index
     RETURN
  END IF
 
  ! Check if table is full
  IF (nspec >= nmaxspec) THEN
!gf
!    WRITE (message_text,*) 'Species list full. nmaxspec = ',nmaxspec
!    CALL message('new_species', 'message_text, level=em_error')
    WRITE (*,*) 'Species list full. nmaxspec = ',nmaxspec
!gf
    RETURN
  END IF
     
  ! Increment number of species instances and store data
  nspec    = nspec    + 1
  i        = nspec
     
  ! set species properties 
  speclist(i)% nphase       = nphase
  speclist(i)% longname     = longname
  speclist(i)% shortname    = shortname
  speclist(i)% units        = units
  speclist(i)% moleweight   = mw

  ! for gas only
  IF (PRESENT(henry))        speclist(i)% henry        = henry

  ! for aerosol only 
  IF (PRESENT(density))      speclist(i)% density      = density
  IF (PRESENT(lelectrolyte)) speclist(i)% lelectrolyte = lelectrolyte
  IF (PRESENT(kappa))        speclist(i)% kappa        = kappa
    
  ! capture species definition errors
  IF (IAND(nphase, AEROSOL) == 0) THEN

     ! capture errors
!gf 
!     IF (PRESENT(density))      CALL message('new_species',  &
!                                  'density undefined for gas-phase species '//TRIM(shortname),   &
!                                  level=em_error)
!     IF (PRESENT(lelectrolyte)) CALL message('new_species',  &
!                                  'lelectrolyte undefined for gas-phase species '//TRIM(shortname), &
!                                  level=em_error)
!     IF (PRESENT(kappa))        CALL message('new_species',  &
!                                  'kappa undefined for gas-phase species '//TRIM(shortname),   &
!                                  level=em_error)
     IF (PRESENT(density)) THEN
        WRITE(*,*) 'new_species: density undefined for gas-phase species '//TRIM(shortname)
        STOP
     ENDIF

     IF (PRESENT(lelectrolyte)) THEN
        WRITE(*,*) 'new_species: electrolyte undefined for gas-phase species '//TRIM(shortname)
        STOP
     ENDIF
 
     IF (PRESENT(kappa)) THEN
        WRITE(*,*) 'new_species: kappa undefined for gas-phase species '//TRIM(shortname)
        STOP
     ENDIF
!gf

  ELSE IF (IAND(nphase, GAS) == 0) THEN

     ! capture errors 
!gf
!     IF (PRESENT(henry))        CALL message('new_species',  &
!                                  'henry undefined for aerosol species '//TRIM(shortname),   &  
!                                level=em_error)
     IF (PRESENT(henry)) THEN
        WRITE(*,*) 'new_species: henry undefined for gas-phase species '//TRIM(shortname)
        STOP
     ENDIF
!gf
  END IF

  ! test validity of aerosol species
  IF (IAND(nphase, AEROSOL) /= 0) THEN
     
     ! add aerosol species to aero_idx list
     naerospec = naerospec + 1
     aero_idx(naerospec) = i

  END IF

  ! Note: no warning if phase is neither gas or aerosol. Nothing will happen: quiet ignorance...
  ! idx will be 0 -- this can be used as indicator for an error

  IF (PRESENT(idx)) idx = i    ! return index

 
  END SUBROUTINE new_species 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>  
!! get properties of a specific species identified by
!! either its longname or shortname. Query by index is ambivalent due to split
!! in gas-phase and aerosol species lists. If you want to know species properties
!! for a given index in speclist, you can get the result directly as 
!! speclist(idx)% <property>.
!! This routine will return ierr/=0 if the species is undefined.
!! 
!! @author see module info 
!!
!! $Id: 1423$
!!
!! @par Revision History
!! see module info 
!!
!! @par This subroutine is called by
!! <ol>
!! <li>new_species 
!! </ol> 
!!
!! @par Externals:
!! <ol>
!! <li>none
!! </ol>
!!
!! @par Notes
!! 
!! @par Responsible coder
!! m.schultz@fz-juelich.de
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE query_species (longname, shortname,   &      ! search criteria
                           index, nphase, units,   &
                           mw, henry, density,     &
                           lelectrolyte, kappa,    &
                           idt, ierr               )

  
  CHARACTER(len=*) ,INTENT(in)  ,OPTIONAL :: longname     ! name of species
  CHARACTER(len=*) ,INTENT(in)  ,OPTIONAL :: shortname    ! subname of species
  INTEGER          ,INTENT(in)  ,OPTIONAL :: nphase       ! GAS or AEROSOL

  INTEGER          ,INTENT(out) ,OPTIONAL :: index        ! index of species as query result
  CHARACTER(len=*) ,INTENT(out) ,OPTIONAL :: units        ! physical units of tracer
  REAL(dp)         ,INTENT(out) ,OPTIONAL :: mw           ! molecular weight
  REAL(dp)         ,INTENT(out) ,OPTIONAL :: henry(2)     ! Henry coefficient

  REAL(dp)         ,INTENT(out) ,OPTIONAL :: density      ! (aerosol) density
  LOGICAL          ,INTENT(out) ,OPTIONAL :: lelectrolyte ! flag for dissociation in water
  REAL(dp)         ,INTENT(out) ,OPTIONAL :: kappa        ! Kappa-Koehler coefficient

  INTEGER          ,INTENT(out) ,OPTIONAL :: idt          ! associated tracer index
  INTEGER          ,INTENT(out) ,OPTIONAL :: ierr         ! error return value

  ! --- local variables
  INTEGER             :: i
  INTEGER             :: ispec   ! local species id 
  INTEGER             :: iphase
  INTEGER, PARAMETER  :: OK = 0
  INTEGER, PARAMETER  :: UNDEF = 1

  ! -- set defaults
  ispec = 0
  IF (PRESENT(ierr))         ierr         = UNDEF       ! default error: species not found
  IF (PRESENT(index))        index        = 0
  IF (PRESENT(units))        units        = ''
  IF (PRESENT(mw))           mw           = 0._dp
  IF (PRESENT(henry))        henry(:)     = 0._dp
  IF (PRESENT(density))      density      = 0._dp
  IF (PRESENT(lelectrolyte)) lelectrolyte = .FALSE.
  IF (PRESENT(kappa))        kappa        = 0._dp
  IF (PRESENT(idt))          idt          = 0

  ! -- test if phase if given
  iphase = -1    ! undefined
  IF (PRESENT(nphase)) THEN
    iphase = nphase
!gf
!    IF (IAND(iphase, GAS+AEROSOL) == 0) CALL message('query_species', &
!                'Invalid value for nphase!', level=em_error)
    IF (IAND(iphase, GAS+AEROSOL) == 0) THEN
       WRITE(*,*) 'query_species:Invalid value for nphase!'
       STOP
    ENDIF
!gf
  END IF

  ! -- search for species: always search for longname first
  IF (PRESENT(longname)) THEN
      DO i=1, nspec
        IF (TRIM(speclist(i)%longname) == TRIM(longname)) THEN 
          ispec = i
          EXIT
        END IF
      END DO
  END IF

  ! -- if no species found or longname not given look for shortname
  IF (ispec == 0 .AND. PRESENT(shortname)) THEN
      DO i=1, nspec
        IF (TRIM(speclist(i)%shortname) == TRIM(shortname)) THEN 
          ispec = i
          EXIT 
        END IF
      END DO
  END IF

  ! --- Collect species properties
  IF (ispec > 0) THEN
    IF (PRESENT(ierr))         ierr         = OK  ! error status: OK
    IF (PRESENT(index))        index        = ispec
    IF (PRESENT(units))        units        = speclist(ispec)% units
    IF (PRESENT(mw))           mw           = speclist(ispec)% moleweight
    IF (PRESENT(henry))        henry(:)     = speclist(ispec)% henry
    IF (PRESENT(density))      density      = speclist(ispec)% density
    IF (PRESENT(lelectrolyte)) lelectrolyte = speclist(ispec)% lelectrolyte
    IF (PRESENT(kappa))        kappa        = speclist(ispec)% kappa
    IF (PRESENT(idt))          idt          = speclist(ispec)% idt
  END IF

  END SUBROUTINE query_species
 
END MODULE mo_species


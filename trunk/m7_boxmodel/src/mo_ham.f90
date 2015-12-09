!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename
!! mo_ham.f90
!!
!! \brief
!! mo_ham contains physical switches and other control parameters for 
!! ECHAM aerosol models (particularly HAM).
!!
!! \author P. Stier (MPI-Met)
!!
!! \responsible_coder
!! Martin Schultz, m.schultz@fz-juelich.de
!!
!! \revision_history
!!   -# P. Stier (MPI-Met) - original version - (2002-12-xx) 
!!   -# K. Zhang (MPI-Met) - changes for ECHAM6 - (2009-08-11)
!!   -# M. Schultz (FZ Juelich) - cleanup (2009-09-23)
!!   -# H. Kokkola (FMI) - definition of aerocomp (former mo_ham mode) (2011-12-12)
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

MODULE mo_ham

  ! *mo_ham* contains physical switches and parameters 
  !           for the ECHAM-HAM aerosol model.
  !
  ! Author:
  ! -------
  ! Philip Stier, MPI-MET                    12/2002
  !

  USE mo_species,             ONLY: t_species

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: setham
  PUBLIC :: new_aerocomp
  PUBLIC :: nmod, naerocomp, nmaxclass

  INTEGER, PARAMETER     :: nmaxclass = 20    ! maximum number of aerosol modes or bins

  !--- derived types
  TYPE, PUBLIC :: t_aeroclass
     INTEGER                  :: iclass         ! Aerosol mode or bin number
     TYPE(t_species), POINTER :: species        ! Aerosol species 
     INTEGER                  :: spid           ! Index in species list
     INTEGER                  :: aero_idx       ! Index to aerosol species list 
     INTEGER                  :: tracer_type    ! Tracer type (none/diagnostic/prognostic)
     INTEGER                  :: idt            ! Tracer identity
  END TYPE t_aeroclass


  TYPE, PUBLIC :: lognormal_mode 
     CHARACTER (LEN=32) :: modename          ! Long mode name, e.g. "nucleation soluble"
     CHARACTER (LEN=2)  :: shortname         ! Short mode name, e.g. "NS"
     INTEGER            :: self              ! =mode index, for quick comparisons, etc
     LOGICAL            :: lsoluble          ! Mode soluble (T/F)
     LOGICAL            :: lsoainmode        ! Secondary organics occur in this mode (T/F)
     INTEGER            :: idt_no            ! Tracer identity for aerosol number
  END TYPE lognormal_mode                    ! sigma, sigmaln and the conversion factors are
                                             ! kept separate to avoid too many impacts on 
                                             ! other modules and subroutines
  INTEGER, SAVE          :: nmod=7           ! number of aerosol modes or size bins (e.g. 7 for M7)
  INTEGER                :: naerocomp        ! number of aerocomps defined (see mo_ham_init)

  TYPE(lognormal_mode), TARGET, PUBLIC :: m7mode(nmaxclass)

  !--- aerocomp: linear list of class*species

  TYPE(t_aeroclass), PUBLIC, ALLOCATABLE :: aerocomp(:)
  TYPE(t_aeroclass), PUBLIC, ALLOCATABLE :: aerowater(:)

  !--- 2) Parameters:

  INTEGER, PARAMETER :: ntype=6
  CHARACTER(LEN=3), PARAMETER :: ctype(ntype)=(/'SO4','BC ','OC ','SS ','DU ','WAT'/)

  LOGICAL, PUBLIC :: lsoa = .FALSE.    ! switch for the secondary organics scheme 
  INTEGER, PUBLIC :: nsoa              ! number of SOA species 

  INTEGER, PUBLIC :: nsoalumping = 0   ! SOA lumping scheme
                                       ! 0: no lumping (DEFAULT: WILL BE CHANGED)
                                       ! 1: lump anthropogenic non-volatile SOA
                                       ! 2: lump anthropogenic non-volatile SOA and map onto OC
                                       ! 3: lump anthropogenic non-volatile SOA and map onto OC
                                       !    and lump all anthropogenic precursors

CONTAINS 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! setham modifies pre-set switches of the hamctl namelist for the 
!! configuration of the ECHAM/HAM aerosol model
!!  
!! @author see above
!!
!! $Id: 1423$
!!
!! @par Revision History
!! see above
!!
!! @par This subroutine is called by
!! init_ham
!!
!! @par Externals:
!! <ol>
!! <li>None
!! </ol>
!!
!! @par Notes
!! 
!!
!! @par Responsible coder
!! kai.zhang@zmaw.de
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  SUBROUTINE setham(nmod)

    ! *setham* modifies pre-set switches of the hamctl
    !           namelist for the configuration of the 
    !           ECHAM/HAM aerosol model
    ! 
    ! Authors:
    ! --------
    ! Philip Stier, MPI-MET                        12/2002
    ! Martin Schultz, FZ Juelich, Oct 2009, added nmod parameter
    !

    USE mo_namelist,            ONLY: open_nml, position_nml, POSITIONED

    IMPLICIT NONE

    INCLUDE 'hamctl.inc'

    INTEGER, INTENT(in)       :: nmod         ! number of modes/bins

    !--- Local variables

    INTEGER                   :: ierr, inml, iunit

    !--- Set defaults for mode-dependent arrays
!gf  
!  IF (nmod > nmaxclass) CALL finish('setham', 'Maximum number of aerosol modes/bins exceeded!')
    IF (nmod > nmaxclass) THEN
       WRITE(*,*) 'setham: Maximum number of aerosol modes/bins exceeded!'
       STOP
    ENDIF
!gf
    !--- 1) Read namelist:

!gf
!    CALL message('setham', 'Reading namelist hamctl...', level=em_debug)
    WRITE(*,*) 'setham: Reading namelist hamctl...'
!gf
       inml = open_nml('namelist.echam')
       iunit = position_nml ('HAMCTL', inml, status=ierr)
       SELECT CASE (ierr)
       CASE (POSITIONED)
          READ (iunit, hamctl)
       CASE DEFAULT 
!gf
!          WRITE(message_text,*) 'Namelist hamctl not correctly read! ierr = ', ierr
!          CALL finish('setham', message_text)
          WRITE(*,*) 'Namelist hamctl not correctly read! ierr = ', ierr
       STOP
!gf
       END SELECT

  END SUBROUTINE setham

  INTEGER FUNCTION new_aerocomp(iclass, ispec)

    USE mo_species,      ONLY: speclist

    IMPLICIT NONE

    !---function interface
    INTEGER, INTENT(IN) :: iclass
    INTEGER, INTENT(IN) :: ispec

    !---local variables
    INTEGER :: i

    naerocomp = naerocomp + 1
          i = naerocomp

    aerocomp(i)%iclass   =  iclass
    aerocomp(i)%spid    =  ispec
    aerocomp(i)%species => speclist(ispec)

    new_aerocomp = i

  END FUNCTION new_aerocomp

END MODULE mo_ham

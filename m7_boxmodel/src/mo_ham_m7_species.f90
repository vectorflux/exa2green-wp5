!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename 
!! mo_ham_m7_species.f90
!!
!! \brief
!! Species mapping from ECHAM species list to condensed gas-phase and aerosol lists in M7
!!
!! \author Martin G. Schultz (FZ Juelich)
!!
!! \responsible_coder
!! Martin G. Schultz, m.schultz@fz-juelich.de
!!
!! \revision_history
!!   -# Martin G. Schultz (FZ Juelich) - original code (2009-10)
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

MODULE mo_ham_m7_species

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: map_m7_species

  PUBLIC :: im7_so2, im7_so4g, im7_so4, im7_wat


  !-- species indices for M7 routines
  INTEGER :: im7_so2, im7_so4g, im7_so4, im7_wat

  CONTAINS

! ---------------------------------------------------------------------------
!  map_m7_species: construct condensed species lists for M7 gas phase and 
!  aerosol species from the general speclist
! ---------------------------------------------------------------------------

  SUBROUTINE map_m7_species

  USE mo_species,         ONLY: nspec
  USE mo_ham,             ONLY: lsoa, nsoa
  USE mo_ham_species,     ONLY: id_so2, id_so4g, id_so4, id_bc,  &
                                id_oc, id_ss, id_du, id_wat
  USE mo_ham_soa,         ONLY: soaprop
  USE mo_ham_m7ctl,       ONLY: immr2molec, immr2ug
                                

  INTEGER         :: jt, jm

  DO jt = 1, nspec
    IF (jt == id_so2)   CALL new_m7_gasspec(id_so2, immr2molec, im7_so2)
    IF (jt == id_so4g)  CALL new_m7_gasspec(id_so4g, immr2molec, im7_so4g)

    IF (jt == id_so4)   CALL new_m7_aerospec(id_so4, immr2molec, im7_so4)
    IF (jt == id_bc)    CALL new_m7_aerospec(id_bc, immr2ug)
    IF (jt == id_oc)    CALL new_m7_aerospec(id_oc, immr2ug)
    IF (jt == id_ss)    CALL new_m7_aerospec(id_ss, immr2ug)
    IF (jt == id_du)    CALL new_m7_aerospec(id_du, immr2ug)
    IF (jt == id_wat)   CALL new_m7_aerospec(id_wat, immr2ug, im7_wat)

    IF (lsoa) THEN
       DO jm = 1,nsoa
          IF (jt == soaprop(jm)%spid_soa) CALL new_m7_aerospec(soaprop(jm)%spid_soa, immr2ug)
       END DO
    END IF

  END DO

  END SUBROUTINE map_m7_species
 
! ---------------------------------------------------------------------------
!  new_m7_gasspec: add a species id of a gas species to the list of species to
!  be considered in M7 processes.
! ---------------------------------------------------------------------------

  SUBROUTINE new_m7_gasspec(nspid, nunitconv, idlocal)

    USE mo_species,             ONLY : nmaxspec
    USE mo_ham_m7ctl,           ONLY : m7_ngasspec, m7_gasspec, m7_gasunitconv

    INTEGER, INTENT(in)             :: nspid, nunitconv
    INTEGER, INTENT(out), OPTIONAL  :: idlocal              ! local species id for M7 routines

    m7_ngasspec = m7_ngasspec + 1
!gf
!    IF (m7_ngasspec > nmaxspec) CALL finish('new_m7_gasspec',   &
!                                         'Number of gas species for M7 (m7_ngasspec) exceeds nmaxspec!')
    IF (m7_ngasspec > nmaxspec) THEN
       WRITE(*,*) 'new_m7_gasspec: Number of gas species for M7 (m7_ngasspec) exceeds nmaxspec!'
       STOP
    ENDIF
!gf
    m7_gasspec(m7_ngasspec)     = nspid
    m7_gasunitconv(m7_ngasspec) = nunitconv

    IF (PRESENT(idlocal)) idlocal = m7_ngasspec

  END SUBROUTINE new_m7_gasspec


! ---------------------------------------------------------------------------
!  new_m7_aerospec: add a species id of a aero species to the list of species to
!  be considered in M7 processes.
! ---------------------------------------------------------------------------

  SUBROUTINE new_m7_aerospec(nspid, nunitconv, idlocal)
  
    USE mo_species,             ONLY : nmaxspec
    USE mo_ham_m7ctl,           ONLY : m7_naerospec, m7_aerospec, m7_aerounitconv, m7_aero_idx
  
    INTEGER, INTENT(in)    :: nspid, nunitconv
    INTEGER, INTENT(out), OPTIONAL  :: idlocal              ! local species id for M7 routines

    m7_naerospec = m7_naerospec + 1 
!gf
!    IF (m7_naerospec > nmaxspec) CALL finish('new_m7_aerospec',        &
!                   'Number of aero species for M7 (m7_ngasspec) exceeds nmaxspec!')
    IF (m7_naerospec > nmaxspec) THEN
       WRITE(*,*) 'new_m7_aerospec: Number of aero species for M7 (m7_ngasspec) exceeds nmaxspec!'
       STOP
    ENDIF
!gf
    m7_aerospec(m7_naerospec)     = nspid 
    m7_aerounitconv(m7_naerospec) = nunitconv
    ! reverse mapping
    m7_aero_idx(nspid) = m7_naerospec
  
    IF (PRESENT(idlocal)) idlocal = m7_naerospec

    END SUBROUTINE new_m7_aerospec

END MODULE mo_ham_m7_species

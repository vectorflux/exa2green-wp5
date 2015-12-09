!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename 
!! hamctl.inc
!!
!! \brief
!! General namelist for HAM
!!
!! \author Philip Stier (MPI-Met)
!! \author Martin G. Schultz (FZ Juelich)
!!
!! \responsible_coder
!! Martin Schultz, m.schultz@fz-juelich.de
!!
!! \revision_history
!!   -# Philip Stier (MPI-Met) - original code (YYYY)
!!   -# Martin G. Schultz (FZ Juelich) (2009)
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

NAMELIST /hamctl/ &
  lsoa,                   & !< Switch for Secondary Organic Aerosols 
  nsoalumping               !< SOA lumping scheme to apply


MODULE mo_constants

!- Description:
!
!  This module contains basic constants and derived constants
!
!- Author:
!
!  M. Giorgetta, MPI, April 1999
!  I. Kirchner, MPI, December 2000, time control
!  L. Kornblueh, MPI, January 2001, cleanup

  USE mo_kind, ONLY: dp

  IMPLICIT NONE

! Universal constants
  REAL(dp), PARAMETER :: api   = 3.14159265358979323846_dp ! pi
  REAL(dp), PARAMETER :: asqrt2= 1.41421356237309504880_dp
  REAL(dp), PARAMETER :: argas = 8.314409_dp      ! universal gas constant (R) in J/K/mol
  REAL(dp), PARAMETER :: avo   = 6.02214e23_dp    ! Avogadro constant in 1/mol

END MODULE mo_constants

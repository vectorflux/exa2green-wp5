! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Sparse Jacobian Data Structures File
! 
! Generated by KPP-2.2 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : small_f90_JacobianSP.f90
! Time                 : Sun Jun  1 10:17:02 2014
! Working directory    : /Users/jlinford/workspace/kppa/verify/small_kpp221
! Equation file        : small_f90.kpp
! Output root filename : small_f90
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE small_f90_JacobianSP

  PUBLIC
  SAVE


! Sparse Jacobian Data


  INTEGER, PARAMETER, DIMENSION(19) :: LU_IROW = (/ &
       1,  1,  2,  2,  2,  2,  3,  3,  3,  3,  3,  4, &
       4,  4,  4,  5,  5,  5,  5 /)

  INTEGER, PARAMETER, DIMENSION(19) :: LU_ICOL = (/ &
       1,  3,  1,  2,  3,  5,  1,  2,  3,  4,  5,  2, &
       3,  4,  5,  2,  3,  4,  5 /)

  INTEGER, PARAMETER, DIMENSION(6) :: LU_CROW = (/ &
       1,  3,  7, 12, 16, 20 /)

  INTEGER, PARAMETER, DIMENSION(6) :: LU_DIAG = (/ &
       1,  4,  9, 14, 19, 20 /)


END MODULE small_f90_JacobianSP


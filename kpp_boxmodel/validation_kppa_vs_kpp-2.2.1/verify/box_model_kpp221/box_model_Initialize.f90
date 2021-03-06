! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Initialization File
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
! File                 : box_model_Initialize.f90
! Time                 : Tue Jun  3 15:01:53 2014
! Working directory    : /Users/jlinford/workspace/kppa/verify/box_model_kpp221
! Equation file        : box_model.kpp
! Output root filename : box_model
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE box_model_Initialize

  USE box_model_Parameters, ONLY: dp, NVAR, NFIX
  IMPLICIT NONE

CONTAINS


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Initialize - function to initialize concentrations
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Initialize ( )


  USE box_model_Global

  INTEGER :: i
  REAL(kind=dp) :: x

  CFACTOR = 2.550000e+10_dp

  x = (1.0E-20)*CFACTOR
  DO i = 1, NVAR
    VAR(i) = x
  END DO

  x = (1.0E-20)*CFACTOR
  DO i = 1, NFIX
    FIX(i) = x
  END DO

  VAR(1) = (78.1E7)*CFACTOR
  VAR(16) = (0)*CFACTOR
  VAR(35) = (2)*CFACTOR
  VAR(39) = (20.9E7)*CFACTOR
  VAR(41) = (100)*CFACTOR
  VAR(43) = (0)*CFACTOR
  VAR(44) = (0.1)*CFACTOR
  VAR(63) = (1)*CFACTOR
  VAR(66) = (1.0E+07)*CFACTOR
  VAR(76) = (0.1)*CFACTOR
  VAR(77) = (0.1)*CFACTOR
  VAR(78) = (30.0)*CFACTOR
  FIX(1) = (1700)*CFACTOR
! constant rate coefficients
  RCONST(26) = 2.14e-10
  RCONST(40) = 2.5e-12
  RCONST(43) = 0
  RCONST(60) = 4e-11
  RCONST(61) = 9e-12
  RCONST(64) = 1.15e-11
  RCONST(65) = 1.7e-11
  RCONST(66) = 2.8e-11
  RCONST(67) = 1e-11
  RCONST(68) = 1e-11
  RCONST(69) = 1e-11
  RCONST(75) = 4.7e-12
  RCONST(96) = 2.2e-11
  RCONST(100) = 5.81e-13
  RCONST(158) = 4e-12
  RCONST(159) = 1.5e-11
  RCONST(162) = 1.2e-12
  RCONST(163) = 1.7e-10
  RCONST(164) = 1.22e-11
  RCONST(165) = 2e-16
  RCONST(166) = 4e-12
  RCONST(167) = 1.5e-11
  RCONST(170) = 1.2e-12
  RCONST(173) = 2e-12
  RCONST(174) = 1e-10
  RCONST(175) = 1.3e-11
  RCONST(180) = 2e-12
  RCONST(183) = 3.6e-12
  RCONST(184) = 3e-11
  RCONST(185) = 3e-12
  RCONST(189) = 5.8e-11
  RCONST(190) = 2.5e-12
  RCONST(191) = 2.5e-12
  RCONST(192) = 2.5e-12
! END constant rate coefficients

! INLINED initializations

  TSTART = 0
  TEND = 86400
  DT = 3600
  TEMP = 270

! End INLINED initializations

      
END SUBROUTINE Initialize

! End of Initialize function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE box_model_Initialize


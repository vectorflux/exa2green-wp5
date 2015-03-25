! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Global Data Module File
! 
! Generated by KPP-2.2.3 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : kpp_Global.f90
! Time                 : Mon Oct 13 15:31:12 2014
! Working directory    : /users/charlesj/KPP_BOXMODEL/extended_box_model_Exa2Green/kpp-2.2.3
! Equation file        : kpp.kpp
! Output root filename : kpp
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE kpp_Global

  USE kpp_Parameters, ONLY: dp, NSPEC, NVAR, NFIX, NREACT, idim, jdim, kdim
  PUBLIC
  SAVE


! Declaration of global variables

! Concentrations of all species for the whole domain
  REAL :: initial_conc_from_file(idim, jdim, kdim, NSPEC)
! Temperatures of all species for the whole domain
  REAL :: temperature_from_file(idim, jdim, kdim)
! C - Concentration of all species
  REAL(kind=dp) :: C(NSPEC)
! VAR - Concentrations of variable species (global)
  REAL(kind=dp) :: VARTOT(NVAR,idim,jdim,kdim)
  REAL(kind=dp) :: VAR(NVAR)
! FIX - Concentrations of fixed species (global)
  REAL(kind=dp) :: FIX(NFIX)
! VAR, FIX are chunks of array C
      EQUIVALENCE( C(1),VAR(1) )
      EQUIVALENCE( C(82),FIX(1) )
! RCONST - Rate constants (global)
  REAL(kind=dp) :: RCONST(NREACT)
  !$OMP THREADPRIVATE(RCONST)
! TIME - Current integration time
  REAL(kind=dp) :: TIME
  !$OMP THREADPRIVATE(TIME)
! SUN - Sunlight intensity between [0,1]
  REAL(kind=dp) :: SUN
! TEMP - Temperature
  REAL(kind=dp) :: TEMP
  !$OMP THREADPRIVATE(TEMP)
! RTOLS - (scalar) Relative tolerance
  REAL(kind=dp) :: RTOLS
! TSTART - Integration start time
  REAL(kind=dp) :: TSTART
! TEND - Integration end time
  REAL(kind=dp) :: TEND
! DT - Integration step
  REAL(kind=dp) :: DT
! ATOL - Absolute tolerance
  REAL(kind=dp) :: ATOL(NVAR)
! RTOL - Relative tolerance
  REAL(kind=dp) :: RTOL(NVAR)
! STEPMIN - Lower bound for integration step
  REAL(kind=dp) :: STEPMIN
  !$OMP THREADPRIVATE(STEPMIN)
! STEPMAX - Upper bound for integration step
  REAL(kind=dp) :: STEPMAX
! CFACTOR - Conversion factor for concentration units
  REAL(kind=dp) :: CFACTOR

! INLINED global variable declarations

  REAL(kind=dp) :: CHI
  REAL(kind=dp) :: M

! INLINED global variable declarations


END MODULE kpp_Global


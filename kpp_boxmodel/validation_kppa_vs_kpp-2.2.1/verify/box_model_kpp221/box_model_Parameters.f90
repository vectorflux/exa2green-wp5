! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Parameter Module File
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
! File                 : box_model_Parameters.f90
! Time                 : Tue Jun  3 15:01:53 2014
! Working directory    : /Users/jlinford/workspace/kppa/verify/box_model_kpp221
! Equation file        : box_model.kpp
! Output root filename : box_model
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE box_model_Parameters

  USE box_model_Precision
  PUBLIC
  SAVE


! NSPEC - Number of chemical species
  INTEGER, PARAMETER :: NSPEC = 82 
! NVAR - Number of Variable species
  INTEGER, PARAMETER :: NVAR = 81 
! NVARACT - Number of Active species
  INTEGER, PARAMETER :: NVARACT = 75 
! NFIX - Number of Fixed species
  INTEGER, PARAMETER :: NFIX = 1 
! NREACT - Number of reactions
  INTEGER, PARAMETER :: NREACT = 192 
! NVARST - Starting of variables in conc. vect.
  INTEGER, PARAMETER :: NVARST = 1 
! NFIXST - Starting of fixed in conc. vect.
  INTEGER, PARAMETER :: NFIXST = 82 
! NONZERO - Number of nonzero entries in Jacobian
  INTEGER, PARAMETER :: NONZERO = 828 
! LU_NONZERO - Number of nonzero entries in LU factoriz. of Jacobian
  INTEGER, PARAMETER :: LU_NONZERO = 971 
! CNVAR - (NVAR+1) Number of elements in compressed row format
  INTEGER, PARAMETER :: CNVAR = 82 
! CNEQN - (NREACT+1) Number stoicm elements in compressed col format
  INTEGER, PARAMETER :: CNEQN = 193 
! NHESS - Length of Sparse Hessian
  INTEGER, PARAMETER :: NHESS = 841 
! NLOOKAT - Number of species to look at
  INTEGER, PARAMETER :: NLOOKAT = 5 
! NMONITOR - Number of species to monitor
  INTEGER, PARAMETER :: NMONITOR = 5 
! NMASS - Number of atoms to check mass balance
  INTEGER, PARAMETER :: NMASS = 1 

! Index declaration for variable species in C and VAR
!   VAR(ind_spc) = C(ind_spc)

  INTEGER, PARAMETER :: ind_N2 = 1 
  INTEGER, PARAMETER :: ind_SULF = 2 
  INTEGER, PARAMETER :: ind_ORA1 = 3 
  INTEGER, PARAMETER :: ind_ORA2 = 4 
  INTEGER, PARAMETER :: ind_CS1 = 5 
  INTEGER, PARAMETER :: ind_CO2 = 6 
  INTEGER, PARAMETER :: ind_H2 = 7 
  INTEGER, PARAMETER :: ind_DMSO = 8 
  INTEGER, PARAMETER :: ind_ETH = 9 
  INTEGER, PARAMETER :: ind_ISHP = 10 
  INTEGER, PARAMETER :: ind_HC5 = 11 
  INTEGER, PARAMETER :: ind_TPAN = 12 
  INTEGER, PARAMETER :: ind_HONO = 13 
  INTEGER, PARAMETER :: ind_HC8 = 14 
  INTEGER, PARAMETER :: ind_MAHP = 15 
  INTEGER, PARAMETER :: ind_DMS = 16 
  INTEGER, PARAMETER :: ind_SO2 = 17 
  INTEGER, PARAMETER :: ind_TOL = 18 
  INTEGER, PARAMETER :: ind_XYL = 19 
  INTEGER, PARAMETER :: ind_HC3 = 20 
  INTEGER, PARAMETER :: ind_NALD = 21 
  INTEGER, PARAMETER :: ind_CS10 = 22 
  INTEGER, PARAMETER :: ind_CS100 = 23 
  INTEGER, PARAMETER :: ind_CS1000 = 24 
  INTEGER, PARAMETER :: ind_O1D = 25 
  INTEGER, PARAMETER :: ind_N2O5 = 26 
  INTEGER, PARAMETER :: ind_MPAN = 27 
  INTEGER, PARAMETER :: ind_OP1 = 28 
  INTEGER, PARAMETER :: ind_PAA = 29 
  INTEGER, PARAMETER :: ind_CSL = 30 
  INTEGER, PARAMETER :: ind_HACE = 31 
  INTEGER, PARAMETER :: ind_HNO4 = 32 
  INTEGER, PARAMETER :: ind_PAN = 33 
  INTEGER, PARAMETER :: ind_O3P = 34 
  INTEGER, PARAMETER :: ind_H2O2 = 35 
  INTEGER, PARAMETER :: ind_OL2 = 36 
  INTEGER, PARAMETER :: ind_ISOP = 37 
  INTEGER, PARAMETER :: ind_ISON = 38 
  INTEGER, PARAMETER :: ind_O2 = 39 
  INTEGER, PARAMETER :: ind_API = 40 
  INTEGER, PARAMETER :: ind_CO = 41 
  INTEGER, PARAMETER :: ind_OLT = 42 
  INTEGER, PARAMETER :: ind_ISO = 43 
  INTEGER, PARAMETER :: ind_HNO3 = 44 
  INTEGER, PARAMETER :: ind_LIM = 45 
  INTEGER, PARAMETER :: ind_GLY = 46 
  INTEGER, PARAMETER :: ind_XNO2 = 47 
  INTEGER, PARAMETER :: ind_XYLP = 48 
  INTEGER, PARAMETER :: ind_MACR = 49 
  INTEGER, PARAMETER :: ind_DCB = 50 
  INTEGER, PARAMETER :: ind_HC5P = 51 
  INTEGER, PARAMETER :: ind_HC8P = 52 
  INTEGER, PARAMETER :: ind_OLIP = 53 
  INTEGER, PARAMETER :: ind_KET = 54 
  INTEGER, PARAMETER :: ind_OL2P = 55 
  INTEGER, PARAMETER :: ind_OLTP = 56 
  INTEGER, PARAMETER :: ind_XO2 = 57 
  INTEGER, PARAMETER :: ind_TOLP = 58 
  INTEGER, PARAMETER :: ind_OP2 = 59 
  INTEGER, PARAMETER :: ind_MACP = 60 
  INTEGER, PARAMETER :: ind_OLN = 61 
  INTEGER, PARAMETER :: ind_MGLY = 62 
  INTEGER, PARAMETER :: ind_HCHO = 63 
  INTEGER, PARAMETER :: ind_TCO3 = 64 
  INTEGER, PARAMETER :: ind_APIP = 65 
  INTEGER, PARAMETER :: ind_H2O = 66 
  INTEGER, PARAMETER :: ind_LIMP = 67 
  INTEGER, PARAMETER :: ind_OLI = 68 
  INTEGER, PARAMETER :: ind_ETHP = 69 
  INTEGER, PARAMETER :: ind_ALD = 70 
  INTEGER, PARAMETER :: ind_KETP = 71 
  INTEGER, PARAMETER :: ind_HO = 72 
  INTEGER, PARAMETER :: ind_HC3P = 73 
  INTEGER, PARAMETER :: ind_ONIT = 74 
  INTEGER, PARAMETER :: ind_HO2 = 75 
  INTEGER, PARAMETER :: ind_NO2 = 76 
  INTEGER, PARAMETER :: ind_NO = 77 
  INTEGER, PARAMETER :: ind_O3 = 78 
  INTEGER, PARAMETER :: ind_MO2 = 79 
  INTEGER, PARAMETER :: ind_NO3 = 80 
  INTEGER, PARAMETER :: ind_ACO3 = 81 

! Index declaration for fixed species in C
!   C(ind_spc)

  INTEGER, PARAMETER :: ind_CH4 = 82 

! Index declaration for fixed species in FIX
!    FIX(indf_spc) = C(ind_spc) = C(NVAR+indf_spc)

  INTEGER, PARAMETER :: indf_CH4 = 1 

! NJVRP - Length of sparse Jacobian JVRP
  INTEGER, PARAMETER :: NJVRP = 352 

! NSTOICM - Length of Sparse Stoichiometric Matrix
  INTEGER, PARAMETER :: NSTOICM = 925 

END MODULE box_model_Parameters


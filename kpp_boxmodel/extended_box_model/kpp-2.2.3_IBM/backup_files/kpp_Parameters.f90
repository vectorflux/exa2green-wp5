! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Parameter Module File
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
! File                 : kpp_Parameters.f90
! Time                 : Mon Oct 13 15:31:12 2014
! Working directory    : /scratch/daint/charlesj/extended_box_model/kpp-2.2.3
! Equation file        : kpp.kpp
! Output root filename : kpp
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE kpp_Parameters

  USE kpp_Precision
  PUBLIC
  SAVE

! Domain size
  INTEGER, PARAMETER :: idim = 222, jdim = 216, kdim = 40

! Start/end indexes of the subgrid in the global grid
  INTEGER, PARAMETER :: idim_loc_s = 65,  jdim_loc_s = 75,  kdim_loc_s = 10
  INTEGER, PARAMETER :: idim_loc_e = 130, jdim_loc_e = 130, kdim_loc_e = 40

! Total number of grid cells
  INTEGER, PARAMETER :: ncells = (kdim_loc_e-kdim_loc_s+1)*(jdim_loc_e-jdim_loc_s+1)*(idim_loc_e-idim_loc_s+1)

! Coordinates of Paris in the global grid
  INTEGER, PARAMETER :: idim_spot = 86, jdim_spot = 108, kdim_spot = 40 
  INTEGER, PARAMETER :: v_spot = (kdim_spot-kdim_loc_s)*(jdim_loc_e-jdim_loc_s+1)*(idim_loc_e-idim_loc_s+1)+(jdim_spot-jdim_loc_s)*(idim_loc_e-idim_loc_s+1)+(idim_spot-idim_loc_s+1)

! NSPEC - Number of chemical species
  INTEGER, PARAMETER :: NSPEC = 83 
! NVAR - Number of Variable species
  INTEGER, PARAMETER :: NVAR = 82 
! NVARACT - Number of Active species
  INTEGER, PARAMETER :: NVARACT = 76 
! NFIX - Number of Fixed species
  INTEGER, PARAMETER :: NFIX = 1 
! NREACT - Number of reactions
  INTEGER, PARAMETER :: NREACT = 194 
! NVARST - Starting of variables in conc. vect.
  INTEGER, PARAMETER :: NVARST = 1 
! NFIXST - Starting of fixed in conc. vect.
  INTEGER, PARAMETER :: NFIXST = 83 
! NONZERO - Number of nonzero entries in Jacobian
  INTEGER, PARAMETER :: NONZERO = 827 
! LU_NONZERO - Number of nonzero entries in LU factoriz. of Jacobian
  INTEGER, PARAMETER :: LU_NONZERO = 968 
! CNVAR - (NVAR+1) Number of elements in compressed row format
  INTEGER, PARAMETER :: CNVAR = 83 
! NLOOKAT - Number of species to look at
  INTEGER, PARAMETER :: NLOOKAT = 8
! NMONITOR - Number of species to monitor
  INTEGER, PARAMETER :: NMONITOR = 8 
! NMASS - Number of atoms to check mass balance
  INTEGER, PARAMETER :: NMASS = 1 

! TAG - Day of simulation in the [1:365] interval
  REAL(kind=dp), PARAMETER :: TAG = 103. ! 13 April 2010

! Index declaration for variable species in C and VAR
!   VAR(ind_spc) = C(ind_spc)

  INTEGER, PARAMETER :: ind_NH3 = 1 
  INTEGER, PARAMETER :: ind_N2 = 2 
  INTEGER, PARAMETER :: ind_SULF = 3 
  INTEGER, PARAMETER :: ind_ORA1 = 4 
  INTEGER, PARAMETER :: ind_ORA2 = 5 
  INTEGER, PARAMETER :: ind_CS1 = 6 
  INTEGER, PARAMETER :: ind_CO2 = 7 
  INTEGER, PARAMETER :: ind_H2 = 8 
  INTEGER, PARAMETER :: ind_ETH = 9 
  INTEGER, PARAMETER :: ind_ISHP = 10 
  INTEGER, PARAMETER :: ind_HC5 = 11 
  INTEGER, PARAMETER :: ind_TPAN = 12 
  INTEGER, PARAMETER :: ind_HONO = 13 
  INTEGER, PARAMETER :: ind_DMSO = 14 
  INTEGER, PARAMETER :: ind_HC8 = 15 
  INTEGER, PARAMETER :: ind_SO2 = 16 
  INTEGER, PARAMETER :: ind_N2O5 = 17 
  INTEGER, PARAMETER :: ind_MAHP = 18 
  INTEGER, PARAMETER :: ind_DMS = 19 
  INTEGER, PARAMETER :: ind_TOL = 20 
  INTEGER, PARAMETER :: ind_XYL = 21 
  INTEGER, PARAMETER :: ind_HC3 = 22 
  INTEGER, PARAMETER :: ind_NALD = 23 
  INTEGER, PARAMETER :: ind_CS10 = 24 
  INTEGER, PARAMETER :: ind_CS100 = 25 
  INTEGER, PARAMETER :: ind_CS1000 = 26 
  INTEGER, PARAMETER :: ind_O1D = 27 
  INTEGER, PARAMETER :: ind_PAA = 28
  INTEGER, PARAMETER :: ind_MPAN = 29
  INTEGER, PARAMETER :: ind_OP1 = 30 
  INTEGER, PARAMETER :: ind_CSL = 31 
  INTEGER, PARAMETER :: ind_HACE = 32 
  INTEGER, PARAMETER :: ind_HNO4 = 33 
  INTEGER, PARAMETER :: ind_PAN = 34 
  INTEGER, PARAMETER :: ind_O3P = 35 
  INTEGER, PARAMETER :: ind_H2O2 = 36 
  INTEGER, PARAMETER :: ind_OL2 = 37 
  INTEGER, PARAMETER :: ind_ISOP = 38 
  INTEGER, PARAMETER :: ind_ISON = 39 
  INTEGER, PARAMETER :: ind_O2 = 40 
  INTEGER, PARAMETER :: ind_API = 41 
  INTEGER, PARAMETER :: ind_CO = 42 
  INTEGER, PARAMETER :: ind_LIM = 43 
  INTEGER, PARAMETER :: ind_OLT = 44 
  INTEGER, PARAMETER :: ind_HNO3 = 45 
  INTEGER, PARAMETER :: ind_ISO = 46 
  INTEGER, PARAMETER :: ind_GLY = 47 
  INTEGER, PARAMETER :: ind_XNO2 = 48 
  INTEGER, PARAMETER :: ind_XYLP = 49 
  INTEGER, PARAMETER :: ind_MACR = 50 
  INTEGER, PARAMETER :: ind_DCB = 51 
  INTEGER, PARAMETER :: ind_HC5P = 52 
  INTEGER, PARAMETER :: ind_HC8P = 53 
  INTEGER, PARAMETER :: ind_OLIP = 54 
  INTEGER, PARAMETER :: ind_KET = 55 
  INTEGER, PARAMETER :: ind_OL2P = 56 
  INTEGER, PARAMETER :: ind_OLTP = 57 
  INTEGER, PARAMETER :: ind_XO2 = 58 
  INTEGER, PARAMETER :: ind_TOLP = 59 
  INTEGER, PARAMETER :: ind_OP2 = 60 
  INTEGER, PARAMETER :: ind_MACP = 61 
  INTEGER, PARAMETER :: ind_OLN = 62 
  INTEGER, PARAMETER :: ind_MGLY = 63 
  INTEGER, PARAMETER :: ind_HCHO = 64 
  INTEGER, PARAMETER :: ind_TCO3 = 65
  INTEGER, PARAMETER :: ind_H2O = 66 
  INTEGER, PARAMETER :: ind_LIMP = 67 
  INTEGER, PARAMETER :: ind_APIP = 68 
  INTEGER, PARAMETER :: ind_OLI = 69 
  INTEGER, PARAMETER :: ind_ONIT = 70 
  INTEGER, PARAMETER :: ind_ALD = 71 
  INTEGER, PARAMETER :: ind_ETHP = 72 
  INTEGER, PARAMETER :: ind_HO2 = 73 
  INTEGER, PARAMETER :: ind_HC3P = 74 
  INTEGER, PARAMETER :: ind_NO2 = 75 
  INTEGER, PARAMETER :: ind_MO2 = 76 
  INTEGER, PARAMETER :: ind_NO = 77 
  INTEGER, PARAMETER :: ind_O3 = 78 
  INTEGER, PARAMETER :: ind_KETP = 79 
  INTEGER, PARAMETER :: ind_NO3 = 80 
  INTEGER, PARAMETER :: ind_ACO3 = 81 
  INTEGER, PARAMETER :: ind_HO = 82

! Index declaration for fixed species in C
!   C(ind_spc)

  INTEGER, PARAMETER :: ind_CH4 = 83 

! Index declaration for fixed species in FIX
!    FIX(indf_spc) = C(ind_spc) = C(NVAR+indf_spc)

  INTEGER, PARAMETER :: indf_CH4 = 1 

! reference solution obtained from a previous reference run of 24h 
! on Piz Daint with KPP-2.2.3, Ros4, rtol = atol = 0.01
  REAL(kind=dp), PARAMETER :: ref_sol(NMONITOR) = &
       (/0.8784400194602625E-02, & ! OP1
       0.9187220281461859E-01, &   ! H2O2
       0.1104451820850433E+01, &   ! HCHO
       0.2311848056029817E+02, &   ! NO2
       0.2784755969096839E-03, &   ! NO
       0.3258921909672916E+01, &   ! O3 
       0.4652958974296319E-03, &   ! NO3
       0.1560354741365994E-06/)    ! HO

END MODULE kpp_Parameters


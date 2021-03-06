!-------------------- BEGIN kppa_CUDA_parameters.f90 BEGIN -------------------
! @file kppa_CUDA_parameters.f90                                              
! @author charlesj                                                            
! @date 2015-07-06 14:41:44.223591                                            
! @brief Program parameters                                                   
!                                                                             
! Integration tolerances, program constants, and species indices.             
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE kppa_CUDA_parameters


  IMPLICIT NONE



!-----------------------------------------------------------------------------
! Integration tolerances                                                      
!-----------------------------------------------------------------------------

  ! Absolute tolerance 
  REAL(8), PARAMETER :: ATOLS = 1.0
  ! Relative tolerance 
  REAL(8), PARAMETER :: RTOLS = 0.001


!-----------------------------------------------------------------------------
! Concentration constants                                                     
!-----------------------------------------------------------------------------

  ! Conversion factor 
  REAL(8), PARAMETER :: CFACTOR = 25500000000.0
  ! Default initialization value for variable species 
  REAL(8), PARAMETER :: VAR_DEFAULT = 1e-20
  ! Default initialization value for fixed species 
  REAL(8), PARAMETER :: FIX_DEFAULT = 1e-20


!-----------------------------------------------------------------------------
! Program constants                                                           
!-----------------------------------------------------------------------------

  ! Species count 
  INTEGER, PARAMETER :: NSPEC = 83
  ! Variable species count 
  INTEGER, PARAMETER :: NVAR = 82
  ! Fixed species count 
  INTEGER, PARAMETER :: NFIX = 1
  ! Active variable species count 
  INTEGER, PARAMETER :: NVARACT = 76
  ! Reaction (equation) count 
  INTEGER, PARAMETER :: NREACT = 194


!-----------------------------------------------------------------------------
! Numerical constants                                                         
!-----------------------------------------------------------------------------

  REAL(8), PARAMETER :: ZERO = 0.0
  REAL(8), PARAMETER :: HALF = 0.5
  REAL(8), PARAMETER :: ONE = 1.0
  REAL(8), PARAMETER :: TWO = 2.0


!-----------------------------------------------------------------------------
! Variable species indices                                                    
!-----------------------------------------------------------------------------

  INTEGER, PARAMETER :: IND_NH3 = 0
  INTEGER, PARAMETER :: IND_N2 = 1
  INTEGER, PARAMETER :: IND_SULF = 2
  INTEGER, PARAMETER :: IND_ORA1 = 3
  INTEGER, PARAMETER :: IND_ORA2 = 4
  INTEGER, PARAMETER :: IND_CS1 = 5
  INTEGER, PARAMETER :: IND_CO2 = 6
  INTEGER, PARAMETER :: IND_H2 = 7
  INTEGER, PARAMETER :: IND_ETH = 8
  INTEGER, PARAMETER :: IND_ISHP = 9
  INTEGER, PARAMETER :: IND_HC5 = 10
  INTEGER, PARAMETER :: IND_TPAN = 11
  INTEGER, PARAMETER :: IND_HONO = 12
  INTEGER, PARAMETER :: IND_DMSO = 13
  INTEGER, PARAMETER :: IND_HC8 = 14
  INTEGER, PARAMETER :: IND_SO2 = 15
  INTEGER, PARAMETER :: IND_N2O5 = 16
  INTEGER, PARAMETER :: IND_MAHP = 17
  INTEGER, PARAMETER :: IND_DMS = 18
  INTEGER, PARAMETER :: IND_TOL = 19
  INTEGER, PARAMETER :: IND_XYL = 20
  INTEGER, PARAMETER :: IND_HC3 = 21
  INTEGER, PARAMETER :: IND_NALD = 22
  INTEGER, PARAMETER :: IND_CS10 = 23
  INTEGER, PARAMETER :: IND_CS100 = 24
  INTEGER, PARAMETER :: IND_CS1000 = 25
  INTEGER, PARAMETER :: IND_O1D = 26
  INTEGER, PARAMETER :: IND_PAA = 27
  INTEGER, PARAMETER :: IND_MPAN = 28
  INTEGER, PARAMETER :: IND_OP1 = 29
  INTEGER, PARAMETER :: IND_CSL = 30
  INTEGER, PARAMETER :: IND_HACE = 31
  INTEGER, PARAMETER :: IND_HNO4 = 32
  INTEGER, PARAMETER :: IND_PAN = 33
  INTEGER, PARAMETER :: IND_O3P = 34
  INTEGER, PARAMETER :: IND_H2O2 = 35
  INTEGER, PARAMETER :: IND_OL2 = 36
  INTEGER, PARAMETER :: IND_ISOP = 37
  INTEGER, PARAMETER :: IND_ISON = 38
  INTEGER, PARAMETER :: IND_O2 = 39
  INTEGER, PARAMETER :: IND_API = 40
  INTEGER, PARAMETER :: IND_CO = 41
  INTEGER, PARAMETER :: IND_LIM = 42
  INTEGER, PARAMETER :: IND_OLT = 43
  INTEGER, PARAMETER :: IND_HNO3 = 44
  INTEGER, PARAMETER :: IND_ISO = 45
  INTEGER, PARAMETER :: IND_GLY = 46
  INTEGER, PARAMETER :: IND_XNO2 = 47
  INTEGER, PARAMETER :: IND_XYLP = 48
  INTEGER, PARAMETER :: IND_MACR = 49
  INTEGER, PARAMETER :: IND_DCB = 50
  INTEGER, PARAMETER :: IND_HC5P = 51
  INTEGER, PARAMETER :: IND_HC8P = 52
  INTEGER, PARAMETER :: IND_OLIP = 53
  INTEGER, PARAMETER :: IND_KET = 54
  INTEGER, PARAMETER :: IND_OL2P = 55
  INTEGER, PARAMETER :: IND_OLTP = 56
  INTEGER, PARAMETER :: IND_XO2 = 57
  INTEGER, PARAMETER :: IND_TOLP = 58
  INTEGER, PARAMETER :: IND_OP2 = 59
  INTEGER, PARAMETER :: IND_MACP = 60
  INTEGER, PARAMETER :: IND_OLN = 61
  INTEGER, PARAMETER :: IND_MGLY = 62
  INTEGER, PARAMETER :: IND_HCHO = 63
  INTEGER, PARAMETER :: IND_TCO3 = 64
  INTEGER, PARAMETER :: IND_H2O = 65
  INTEGER, PARAMETER :: IND_LIMP = 66
  INTEGER, PARAMETER :: IND_APIP = 67
  INTEGER, PARAMETER :: IND_OLI = 68
  INTEGER, PARAMETER :: IND_ONIT = 69
  INTEGER, PARAMETER :: IND_ALD = 70
  INTEGER, PARAMETER :: IND_ETHP = 71
  INTEGER, PARAMETER :: IND_HO2 = 72
  INTEGER, PARAMETER :: IND_HC3P = 73
  INTEGER, PARAMETER :: IND_NO2 = 74
  INTEGER, PARAMETER :: IND_MO2 = 75
  INTEGER, PARAMETER :: IND_NO = 76
  INTEGER, PARAMETER :: IND_O3 = 77
  INTEGER, PARAMETER :: IND_KETP = 78
  INTEGER, PARAMETER :: IND_NO3 = 79
  INTEGER, PARAMETER :: IND_ACO3 = 80
  INTEGER, PARAMETER :: IND_HO = 81


!-----------------------------------------------------------------------------
! Fixed species indices                                                       
!-----------------------------------------------------------------------------

  INTEGER, PARAMETER :: IND_CH4 = 82


END MODULE kppa_CUDA_parameters
!---------------------- END kppa_CUDA_parameters.f90 END ---------------------

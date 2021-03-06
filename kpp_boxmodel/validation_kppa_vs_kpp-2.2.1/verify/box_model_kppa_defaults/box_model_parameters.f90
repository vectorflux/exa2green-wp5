!-------------------- BEGIN box_model_parameters.f90 BEGIN -------------------
! @file box_model_parameters.f90                                              
! @author jlinford                                                            
! @date 2014-06-03 14:55:30.886652                                            
! @brief Program parameters                                                   
!                                                                             
! Integration tolerances, program constants, and species indices.             
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE box_model_parameters


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
  INTEGER, PARAMETER :: NSPEC = 82
  ! Variable species count 
  INTEGER, PARAMETER :: NVAR = 81
  ! Fixed species count 
  INTEGER, PARAMETER :: NFIX = 1
  ! Active variable species count 
  INTEGER, PARAMETER :: NVARACT = 75
  ! Reaction (equation) count 
  INTEGER, PARAMETER :: NREACT = 192


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

  INTEGER, PARAMETER :: IND_N2 = 0
  INTEGER, PARAMETER :: IND_SULF = 1
  INTEGER, PARAMETER :: IND_ORA1 = 2
  INTEGER, PARAMETER :: IND_ORA2 = 3
  INTEGER, PARAMETER :: IND_CS1 = 4
  INTEGER, PARAMETER :: IND_CO2 = 5
  INTEGER, PARAMETER :: IND_H2 = 6
  INTEGER, PARAMETER :: IND_DMSO = 7
  INTEGER, PARAMETER :: IND_ETH = 8
  INTEGER, PARAMETER :: IND_ISHP = 9
  INTEGER, PARAMETER :: IND_HC5 = 10
  INTEGER, PARAMETER :: IND_TPAN = 11
  INTEGER, PARAMETER :: IND_HONO = 12
  INTEGER, PARAMETER :: IND_HC8 = 13
  INTEGER, PARAMETER :: IND_MAHP = 14
  INTEGER, PARAMETER :: IND_DMS = 15
  INTEGER, PARAMETER :: IND_SO2 = 16
  INTEGER, PARAMETER :: IND_TOL = 17
  INTEGER, PARAMETER :: IND_XYL = 18
  INTEGER, PARAMETER :: IND_HC3 = 19
  INTEGER, PARAMETER :: IND_NALD = 20
  INTEGER, PARAMETER :: IND_CS10 = 21
  INTEGER, PARAMETER :: IND_CS100 = 22
  INTEGER, PARAMETER :: IND_CS1000 = 23
  INTEGER, PARAMETER :: IND_O1D = 24
  INTEGER, PARAMETER :: IND_N2O5 = 25
  INTEGER, PARAMETER :: IND_MPAN = 26
  INTEGER, PARAMETER :: IND_OP1 = 27
  INTEGER, PARAMETER :: IND_PAA = 28
  INTEGER, PARAMETER :: IND_CSL = 29
  INTEGER, PARAMETER :: IND_HACE = 30
  INTEGER, PARAMETER :: IND_HNO4 = 31
  INTEGER, PARAMETER :: IND_PAN = 32
  INTEGER, PARAMETER :: IND_O3P = 33
  INTEGER, PARAMETER :: IND_H2O2 = 34
  INTEGER, PARAMETER :: IND_OL2 = 35
  INTEGER, PARAMETER :: IND_ISOP = 36
  INTEGER, PARAMETER :: IND_ISON = 37
  INTEGER, PARAMETER :: IND_O2 = 38
  INTEGER, PARAMETER :: IND_API = 39
  INTEGER, PARAMETER :: IND_CO = 40
  INTEGER, PARAMETER :: IND_OLT = 41
  INTEGER, PARAMETER :: IND_ISO = 42
  INTEGER, PARAMETER :: IND_HNO3 = 43
  INTEGER, PARAMETER :: IND_LIM = 44
  INTEGER, PARAMETER :: IND_GLY = 45
  INTEGER, PARAMETER :: IND_XNO2 = 46
  INTEGER, PARAMETER :: IND_XYLP = 47
  INTEGER, PARAMETER :: IND_MACR = 48
  INTEGER, PARAMETER :: IND_DCB = 49
  INTEGER, PARAMETER :: IND_HC5P = 50
  INTEGER, PARAMETER :: IND_HC8P = 51
  INTEGER, PARAMETER :: IND_OLIP = 52
  INTEGER, PARAMETER :: IND_KET = 53
  INTEGER, PARAMETER :: IND_OL2P = 54
  INTEGER, PARAMETER :: IND_OLTP = 55
  INTEGER, PARAMETER :: IND_XO2 = 56
  INTEGER, PARAMETER :: IND_TOLP = 57
  INTEGER, PARAMETER :: IND_OP2 = 58
  INTEGER, PARAMETER :: IND_MACP = 59
  INTEGER, PARAMETER :: IND_OLN = 60
  INTEGER, PARAMETER :: IND_MGLY = 61
  INTEGER, PARAMETER :: IND_HCHO = 62
  INTEGER, PARAMETER :: IND_TCO3 = 63
  INTEGER, PARAMETER :: IND_APIP = 64
  INTEGER, PARAMETER :: IND_H2O = 65
  INTEGER, PARAMETER :: IND_LIMP = 66
  INTEGER, PARAMETER :: IND_OLI = 67
  INTEGER, PARAMETER :: IND_ETHP = 68
  INTEGER, PARAMETER :: IND_ALD = 69
  INTEGER, PARAMETER :: IND_KETP = 70
  INTEGER, PARAMETER :: IND_HO = 71
  INTEGER, PARAMETER :: IND_HC3P = 72
  INTEGER, PARAMETER :: IND_ONIT = 73
  INTEGER, PARAMETER :: IND_HO2 = 74
  INTEGER, PARAMETER :: IND_NO2 = 75
  INTEGER, PARAMETER :: IND_NO = 76
  INTEGER, PARAMETER :: IND_O3 = 77
  INTEGER, PARAMETER :: IND_MO2 = 78
  INTEGER, PARAMETER :: IND_NO3 = 79
  INTEGER, PARAMETER :: IND_ACO3 = 80


!-----------------------------------------------------------------------------
! Fixed species indices                                                       
!-----------------------------------------------------------------------------

  INTEGER, PARAMETER :: IND_CH4 = 81


END MODULE box_model_parameters
!---------------------- END box_model_parameters.f90 END ---------------------

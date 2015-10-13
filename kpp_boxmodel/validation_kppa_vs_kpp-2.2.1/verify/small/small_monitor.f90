!----------------------- BEGIN small_monitor.f90 BEGIN -----------------------
! @file small_monitor.f90                                                     
! @author jlinford                                                            
! @date 2014-06-01 10:26:33.352871                                            
! @brief Mechanism monitoring                                                 
!                                                                             
! Species names, equations, etc.                                              
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE small_monitor

  USE small_parameters

  IMPLICIT NONE



!-----------------------------------------------------------------------------
! Array dimensions                                                            
!-----------------------------------------------------------------------------

  ! Number of species on the LOOKAT list 
  INTEGER, PARAMETER :: NLOOKAT = 7

  ! Number of species on the MONITOR list 
  INTEGER, PARAMETER :: NMONITOR = 6

!-----------------------------------------------------------------------------
! Species indices arrays                                                      
!-----------------------------------------------------------------------------

  ! Indices of species concentrations sent to data file 
  INTEGER, PARAMETER :: ILOOKAT(NLOOKAT) = (/ 1, 2, 3, 4, 5, 6, 7 /)

  ! Indicies of species concentrations sent to screen 
  INTEGER, PARAMETER :: IMONITOR(NMONITOR) = (/ 1, 2, 3, 4, 5, 7 /)

!-----------------------------------------------------------------------------
! Mechanism information                                                       
!-----------------------------------------------------------------------------

  ! Species names 
  CHARACTER(LEN=3), PARAMETER :: SPC_NAMES(NSPEC) = (/ &
      'O1D', &
      'O  ', &
      'O3 ', &
      'NO ', &
      'NO2', &
      'M  ', &
      'O2 '  /)

  ! Equations 
  CHARACTER(LEN=28), PARAMETER :: EQN_NAMES(NREACT) = (/ &
      '<R1>    O2 + hv --> 2 O     ', &
      '<R2>    O + O2 --> O3       ', &
      '<R3>    O3 + hv --> O + O2  ', &
      '<R4>    O + O3 --> 2 O2     ', &
      '<R5>    O3 + hv --> O1D + O2', &
      '<R6>    O1D + M --> O + M   ', &
      '<R7>    O1D + O3 --> 2 O2   ', &
      '<R8>    NO + O3 --> NO2 + O2', &
      '<R9>    NO2 + O --> NO + O2 ', &
      '<R10>   NO2 + hv --> NO + O '  /)


END MODULE small_monitor
!------------------------- END small_monitor.f90 END -------------------------
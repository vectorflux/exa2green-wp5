!---------------- BEGIN box_model_driver_parameters.f90 BEGIN ----------------
! @file box_model_driver_parameters.f90                                       
! @author charlesj                                                            
! @date 2014-10-07 17:25:54.600889                                            
! @brief Program driver parameters                                            
!                                                                             
! Time domain definitions and grid cell count                                 
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE box_model_driver_parameters


  IMPLICIT NONE



!-----------------------------------------------------------------------------
! Time domain                                                                 
!-----------------------------------------------------------------------------

  ! Integration start time 
  REAL(8), PARAMETER :: TSTART = 0.0
  ! Integration end time 
  REAL(8), PARAMETER :: TEND = 3600.0
  ! Time between integrator restarts 
  REAL(8), PARAMETER :: TDEL = 120.0


!-----------------------------------------------------------------------------
! Grid dimensions                                                             
!-----------------------------------------------------------------------------

  ! Start indexes of the subgrid in the global grid
  INTEGER, PARAMETER :: GRID0_LOC_S = 65
  INTEGER, PARAMETER :: GRID1_LOC_S = 75
  INTEGER, PARAMETER :: GRID2_LOC_S = 10

  ! End indexes of the subgrid in the global grid
  INTEGER, PARAMETER :: GRID0_LOC_E = 130
  INTEGER, PARAMETER :: GRID1_LOC_E = 130
  INTEGER, PARAMETER :: GRID2_LOC_E = 40

  ! Coordinates of Paris in the local grid
  INTEGER, PARAMETER :: GRID0_SPOT = 22
  INTEGER, PARAMETER :: GRID1_SPOT = 34
  INTEGER, PARAMETER :: GRID2_SPOT = 31

  ! Number of grid cells to integrate 
  INTEGER, PARAMETER :: NCELLS = (GRID0_LOC_E-GRID0_LOC_S+1)*(GRID1_LOC_E-GRID1_LOC_S+1)*(GRID2_LOC_E-GRID2_LOC_S+1)

END MODULE box_model_driver_parameters
!------------------ END box_model_driver_parameters.f90 END ------------------

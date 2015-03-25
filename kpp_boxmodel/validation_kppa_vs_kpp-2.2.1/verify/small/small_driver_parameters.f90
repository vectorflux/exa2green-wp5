!------------------ BEGIN small_driver_parameters.f90 BEGIN ------------------
! @file small_driver_parameters.f90                                           
! @author jlinford                                                            
! @date 2014-06-01 10:26:33.343561                                            
! @brief Program driver parameters                                            
!                                                                             
! Time domain definitions and grid cell count                                 
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE small_driver_parameters


  IMPLICIT NONE



!-----------------------------------------------------------------------------
! Time domain                                                                 
!-----------------------------------------------------------------------------

  ! Integration start time 
  REAL(8), PARAMETER :: TSTART = 43200.0
  ! Integration end time 
  REAL(8), PARAMETER :: TEND = 302400.0
  ! Time between integrator restarts 
  REAL(8), PARAMETER :: TDEL = 900.0


!-----------------------------------------------------------------------------
! Grid dimensions                                                             
!-----------------------------------------------------------------------------

  ! Grid dimension 0 
  INTEGER, PARAMETER :: GRID0 = 1
  ! Grid dimension 1 
  INTEGER, PARAMETER :: GRID1 = 1
  ! Grid dimension 2 
  INTEGER, PARAMETER :: GRID2 = 1
  ! Number of grid cells to integrate 
  INTEGER, PARAMETER :: NCELLS = GRID0*GRID1*GRID2



END MODULE small_driver_parameters
!-------------------- END small_driver_parameters.f90 END --------------------

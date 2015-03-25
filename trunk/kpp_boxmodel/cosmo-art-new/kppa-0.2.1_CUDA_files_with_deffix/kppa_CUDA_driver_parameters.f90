!---------------- BEGIN kppa_CUDA_driver_parameters.f90 BEGIN ----------------
! @file kppa_CUDA_driver_parameters.f90                                       
! @author charlesj                                                            
! @date 2015-01-22 16:21:36.539657                                            
! @brief Program driver parameters                                            
!                                                                             
! Time domain definitions and grid cell count                                 
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE kppa_CUDA_driver_parameters


  IMPLICIT NONE



!-----------------------------------------------------------------------------
! Time domain                                                                 
!-----------------------------------------------------------------------------

  ! Integration start time 
  REAL(8), PARAMETER :: TSTART = 0
  ! Integration end time 
  REAL(8), PARAMETER :: TEND = 172800
  ! Time between integrator restarts 
  REAL(8), PARAMETER :: TDEL = 120.0


!-----------------------------------------------------------------------------
! Grid dimensions                                                             
!-----------------------------------------------------------------------------

  ! Grid dimension 0 
  INTEGER, PARAMETER :: GRID0 = 222
  ! Grid dimension 1 
  INTEGER, PARAMETER :: GRID1 = 216
  ! Grid dimension 2 
  INTEGER, PARAMETER :: GRID2 = 40
  ! Number of grid cells to integrate 
  INTEGER, PARAMETER :: NCELLS = GRID0*GRID1*GRID2



END MODULE kppa_CUDA_driver_parameters
!------------------ END kppa_CUDA_driver_parameters.f90 END ------------------

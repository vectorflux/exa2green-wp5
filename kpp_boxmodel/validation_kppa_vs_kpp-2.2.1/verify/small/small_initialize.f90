!---------------------- BEGIN small_initialize.f90 BEGIN ---------------------
! @file small_initialize.f90                                                  
! @author jlinford                                                            
! @date 2014-06-01 10:26:33.335123                                            
! @brief Species concentration initialization                                 
!                                                                             
! Species concentration initialization                                        
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE small_initialize

  USE small_parameters

  IMPLICIT NONE





  CONTAINS

!--------------------------------- Initialize --------------------------------
! Species concentration initialization.                                       
! Concentrations are usually supplied by the model from input data            
! or other calculations, not specified directly as done here.                 
!                                                                             
! @param[in,out] var Variable species concentrations                          
! @param[in,out] fix Fixed species concentrations                             
!-----------------------------------------------------------------------------
  SUBROUTINE Initialize(var, fix)
    IMPLICIT NONE

    REAL(8), INTENT(INOUT) :: var(5)
    REAL(8), INTENT(INOUT) :: fix(2)

    ! Variable species initialization 
    var(1) = 99.06*CFACTOR
    var(2) = 662400000.0*CFACTOR
    var(3) = 532600000000.0*CFACTOR
    var(4) = 872500000.0*CFACTOR
    var(5) = 224000000.0*CFACTOR

    ! Fixed species initialization 
    fix(1) = 8.12e+16*CFACTOR
    fix(2) = 1.697e+16*CFACTOR
  END SUBROUTINE Initialize


END MODULE small_initialize
!------------------------ END small_initialize.f90 END -----------------------

!------------------------ BEGIN small_decomp.f90 BEGIN -----------------------
! @file small_decomp.f90                                                      
! @author jlinford                                                            
! @date 2014-06-01 10:26:33.223654                                            
! @brief LU decomposition of the row-compressed sparse Jacobian               
!                                                                             
! LU decomposition of the row-compressed sparse Jacobian                      
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE small_decomp

  USE small_parameters
  USE small_sparse

  IMPLICIT NONE




  CONTAINS

!----------------------------------- Decomp ----------------------------------
! In-place sparse LU decomposition                                            
!                                                                             
! @param[in,out] A Row-compressed matrix with zero fill                       
!-----------------------------------------------------------------------------
  INTEGER FUNCTION Decomp(A)
    IMPLICIT NONE

    REAL(8), INTENT(INOUT) :: A(JAC_LU_NZ)

    REAL(8) :: t0
    REAL(8) :: t1
    REAL(8) :: t2
    REAL(8) :: t3

    t0 = A(3)/A(1)
    A(3) = t0
    A(5) = -A(2)*t0 + A(5)
    t1 = A(7)/A(1)
    A(7) = t1
    A(9) = -A(2)*t1 + A(9)
    t2 = A(8)/A(4)
    A(8) = t2
    A(9) = -A(5)*t2 + A(9)
    A(11) = A(11) - A(6)*t2
    t3 = A(12)/A(4)
    A(12) = t3
    A(13) = A(13) - A(5)*t3
    A(15) = A(15) - A(6)*t3
    t0 = A(13)/A(9)
    A(13) = t0
    A(14) = -A(10)*t0 + A(14)
    A(15) = -A(11)*t0 + A(15)
    t1 = A(16)/A(4)
    A(16) = t1
    A(17) = A(17) - A(5)*t1
    A(19) = A(19) - A(6)*t1
    t2 = A(17)/A(9)
    A(17) = t2
    A(18) = -A(10)*t2 + A(18)
    A(19) = -A(11)*t2 + A(19)
    t3 = A(18)/A(14)
    A(18) = t3
    A(19) = -A(15)*t3 + A(19)

    Decomp = 0
    RETURN
  END FUNCTION Decomp



END MODULE small_decomp
!-------------------------- END small_decomp.f90 END -------------------------
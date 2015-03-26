!------------------------ BEGIN small_sparse.f90 BEGIN -----------------------
! @file small_sparse.f90                                                      
! @author jlinford                                                            
! @date 2014-06-01 10:26:33.195756                                            
! @brief Data and utilities for row-compressed sparse matrices                
!                                                                             
! The following matrices are represented here in row-compressed form:         
! @li The Jacobian                                                            
! @li The LU decomposition of the Jacobian                                    
! @li The stoichiometric matrix                                               
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE small_sparse


  IMPLICIT NONE



  ! Number of nonzero entries in the Jacobian 
  INTEGER, PARAMETER :: JAC_NZ = 19

  ! Number of nonzero entries in the LU decomposition of the Jacobian 
  INTEGER, PARAMETER :: JAC_LU_NZ = 19

  ! Number of nonzero entries in the stoichiometric matrix 
  INTEGER, PARAMETER :: STOICH_NZ = 34

  ! Number of nonzero entries in the left-side stoichiometric matrix 
  INTEGER, PARAMETER :: LHS_STOICH_NZ = 20

  ! Number of nonzero entries in the right-side stoichiometric matrix 
  INTEGER, PARAMETER :: RHS_STOICH_NZ = 16


  ! Row indices of elements in the row-compressed Jacobian 
  INTEGER, PARAMETER :: JAC_IROW(19) = (/ 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, &
      4, 4, 4, 5, 5, 5, 5 /)

  ! Column indices of elements in the row-compressed Jacobian 
  INTEGER, PARAMETER :: JAC_ICOL(19) = (/ 1, 3, 1, 2, 3, 5, 1, 2, 3, 4, 5, 2, &
      3, 4, 5, 2, 3, 4, 5 /)

  ! Start-of-row indices in the row-compressed Jacobian 
  INTEGER, PARAMETER :: JAC_CROW(6) = (/ 1, 3, 7, 12, 16, 20 /)

  ! Diagonal indices in the row-compressed Jacobian 
  INTEGER, PARAMETER :: JAC_DIAG(6) = (/ 1, 4, 9, 14, 19, 20 /)

  ! Row indices of elements in the row-compressed LU decomposition of the Jacobian 
  INTEGER, PARAMETER :: JAC_LU_IROW(19) = (/ 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, &
      4, 4, 4, 4, 5, 5, 5, 5 /)

  ! Column indices of elements in the row-compressed LU decomposition of the Jacobian 
  INTEGER, PARAMETER :: JAC_LU_ICOL(19) = (/ 1, 3, 1, 2, 3, 5, 1, 2, 3, 4, 5, &
      2, 3, 4, 5, 2, 3, 4, 5 /)

  ! Start-of-row indices in the row-compressed LU decomposition of the Jacobian 
  INTEGER, PARAMETER :: JAC_LU_CROW(6) = (/ 1, 3, 7, 12, 16, 20 /)

  ! Diagonal indices in the row-compressed LU decomposition of the Jacobian 
  INTEGER, PARAMETER :: JAC_LU_DIAG(6) = (/ 1, 4, 9, 14, 19, 20 /)

  ! Row indices of elements in the row-compressed stoichiometric matrix 
  INTEGER, PARAMETER :: STOICH_IROW(34) = (/ 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, &
      3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8 /)

  ! Column indices of elements in the row-compressed stoichiometric matrix 
  INTEGER, PARAMETER :: STOICH_ICOL(34) = (/ 5, 6, 7, 1, 2, 3, 4, 6, 9, 10, &
      2, 3, 4, 5, 7, 8, 8, 9, 10, 8, 9, 10, 1, 2, 3, 4, 5, 7, 8, 9, 1, 3, 5, &
      10 /)

  ! Start-of-row indices in the row-compressed stoichiometric matrix 
  INTEGER, PARAMETER :: STOICH_CROW(10) = (/ 1, 4, 11, 17, 20, 23, 23, 31, &
      35, 35 /)

  ! Stoichiometric coefficients 
  REAL(8), PARAMETER :: STOICH(34) = (/ 1.0, -1.0, -1.0, 2.0, -1.0, 1.0, &
      -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 1.0, &
      1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 2.0, 1.0, 2.0, 1.0, 1.0, -1.0, &
      -1.0, -1.0, -1.0 /)

  ! Row indices of elements in the row-compressed left-side stoichiometric matrix 
  INTEGER, PARAMETER :: LHS_STOICH_IROW(20) = (/ 1, 1, 2, 2, 2, 3, 3, 3, 3, &
      3, 4, 5, 5, 6, 7, 7, 8, 8, 8, 8 /)

  ! Column indices of elements in the row-compressed left-side stoichiometric matrix 
  INTEGER, PARAMETER :: LHS_STOICH_ICOL(20) = (/ 6, 7, 2, 4, 9, 3, 4, 5, 7, &
      8, 8, 9, 10, 6, 1, 2, 1, 3, 5, 10 /)

  ! Start-of-row indices in the row-compressed left-side stoichiometric matrix 
  INTEGER, PARAMETER :: LHS_STOICH_CROW(10) = (/ 1, 3, 6, 11, 12, 14, 15, 17, &
      21, 21 /)

  ! Left-side stoichiometric coefficients 
  REAL(8), PARAMETER :: LHS_STOICH(20) = (/ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 /)

  ! Row indices of elements in the row-compressed right-side stoichiometric matrix 
  INTEGER, PARAMETER :: RHS_STOICH_IROW(16) = (/ 1, 2, 2, 2, 2, 3, 4, 4, 5, &
      6, 7, 7, 7, 7, 7, 7 /)

  ! Column indices of elements in the row-compressed right-side stoichiometric matrix 
  INTEGER, PARAMETER :: RHS_STOICH_ICOL(16) = (/ 5, 1, 3, 6, 10, 2, 9, 10, 8, &
      6, 3, 4, 5, 7, 8, 9 /)

  ! Start-of-row indices in the row-compressed right-side stoichiometric matrix 
  INTEGER, PARAMETER :: RHS_STOICH_CROW(10) = (/ 1, 2, 6, 7, 9, 10, 11, 17, &
      17, 17 /)

  ! Right-side stoichiometric coefficients 
  REAL(8), PARAMETER :: RHS_STOICH(16) = (/ 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, &
      1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, 1.0, 1.0 /)



  CONTAINS

!------------------------------------ CSR ------------------------------------
! Retrieves an element from a compressed sparse row matrix                    
!                                                                             
! @param[in]     nz   Matrix nonzero values                                   
! @param[in]     crow Row start indices                                       
! @param[in]     icol Column indices                                          
! @param[in]     row  Row index into matrix                                   
! @param[in]     col  Column index into matrix                                
!-----------------------------------------------------------------------------
  REAL(8) FUNCTION CSR(nz, crow, icol, row, col)
    IMPLICIT NONE

    REAL(8), INTENT(IN) :: nz(:)
    INTEGER, INTENT(IN) :: crow(:)
    INTEGER, INTENT(IN) :: icol(:)
    INTEGER, INTENT(IN) :: row
    INTEGER, INTENT(IN) :: col

    ! i-loop index 
    INTEGER :: i

    i = crow(row)
    DO WHILE (i < crow(row + 1))
      IF (icol(i) == col) THEN
        CSR = nz(i)
        RETURN
      END IF ! icol(i) == col 
      i = i + 1
    END DO ! i < crow(row + 1) 
    CSR = 0
    RETURN
  END FUNCTION CSR



END MODULE small_sparse
!-------------------------- END small_sparse.f90 END -------------------------
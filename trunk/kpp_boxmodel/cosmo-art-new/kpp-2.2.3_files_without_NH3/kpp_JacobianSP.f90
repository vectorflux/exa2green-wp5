! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Sparse Jacobian Data Structures File
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
! File                 : kpp_JacobianSP.f90
! Time                 : Wed Feb 18 15:16:05 2015
! Working directory    : /users/charlesj/KPP_BOXMODEL/cosmo-art-new/kpp-2.2.3_files_without_NH3
! Equation file        : kpp.kpp
! Output root filename : kpp
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE kpp_JacobianSP

  PUBLIC
  SAVE


! Sparse Jacobian Data


  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_0 = (/ &
       1,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3, &
       4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4, &
       4,  4,  4,  4,  4,  5,  5,  5,  5,  5,  5,  5, &
       5,  5,  5,  5,  5,  6,  6,  7,  7,  8,  8,  9, &
       9,  9,  9, 10, 10, 11, 11, 11, 12, 12, 12, 13, &
      13, 13, 14, 14, 15, 15, 15, 16, 16, 16, 16, 17, &
      17, 17, 18, 18, 18, 18, 18, 19, 19, 20, 20, 21, &
      21, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, &
      23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, &
      24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, &
      25, 25, 25, 26, 26, 26, 26, 26, 27, 27, 27, 27, &
      28, 28, 28, 28, 29, 29, 29, 29, 30, 30, 30, 30, &
      30, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, &
      32, 33, 33, 33, 33, 34, 34, 34, 34, 35, 35, 35, &
      35, 35, 35, 35, 35, 36, 36, 36, 36, 36, 36, 36, &
      36, 37, 37, 37, 37, 38, 38, 38, 38, 38, 39, 39, &
      39, 39, 39, 39, 39, 40, 40, 40, 40, 40, 40, 40, &
      40, 40, 40, 41, 41, 41, 41, 41, 41, 41, 41, 41, &
      41, 41, 41, 42, 42, 42, 42, 43, 43, 43, 43, 43, &
      43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, &
      43, 43, 43, 43, 43, 43, 44, 44, 44, 44, 45, 45, &
      45, 45, 45, 46, 46, 46, 46, 47, 47, 47, 47, 47, &
      47, 47, 47, 48, 48, 48, 48, 48, 48, 48, 48, 49, &
      49, 49, 49, 49, 49, 49, 50, 50, 50, 50, 50, 50, &
      50, 50, 50, 50, 50, 50, 50, 51, 51, 51, 51, 51, &
      51, 51, 51, 51, 52, 52, 52, 52, 52, 52, 52, 53, &
      53, 53, 53, 53, 53, 53, 54, 54, 54, 54, 54, 54, &
      54, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, &
      55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, &
      56, 56, 56, 57, 57, 57, 57, 57, 57, 57, 57, 57 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_IROW_1 = (/ &
      58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58, &
      58, 59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 60, &
      60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, &
      60, 60, 60, 60, 60, 60, 60, 60, 60, 61, 61, 61, &
      61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 62, &
      62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, &
      63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, &
      63, 63, 63, 63, 63, 63, 63, 64, 64, 64, 64, 64, &
      64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, &
      64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, &
      64, 64, 64, 64, 64, 64, 64, 64, 64, 65, 65, 65, &
      65, 65, 65, 65, 65, 65, 65, 65, 65, 66, 66, 66, &
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, &
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, &
      66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, &
      67, 67, 67, 67, 67, 67, 67, 67, 67, 68, 68, 68, &
      68, 68, 68, 68, 68, 68, 69, 69, 69, 69, 69, 69, &
      69, 69, 69, 70, 70, 70, 70, 70, 70, 70, 70, 70, &
      70, 70, 70, 70, 70, 70, 71, 71, 71, 71, 71, 71, &
      71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, &
      71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, &
      72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, &
      72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, &
      73, 73, 73, 73, 73, 73, 74, 74, 74, 74, 74, 74, &
      74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, &
      74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, &
      74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, &
      74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 75, 75, &
      75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, &
      75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75 /)
  INTEGER, PARAMETER, DIMENSION(255) :: LU_IROW_2 = (/ &
      75, 75, 75, 75, 75, 75, 76, 76, 76, 76, 76, 76, &
      76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, &
      77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, &
      77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, &
      77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, &
      77, 77, 77, 77, 77, 77, 77, 77, 77, 78, 78, 78, &
      78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, &
      78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, &
      78, 78, 78, 78, 78, 78, 79, 79, 79, 79, 79, 79, &
      79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, &
      79, 79, 79, 79, 79, 79, 79, 80, 80, 80, 80, 80, &
      80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, &
      80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, &
      80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, &
      80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, &
      80, 80, 80, 80, 80, 80, 80, 80, 81, 81, 81, 81, &
      81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, &
      81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, &
      81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, &
      82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, &
      82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, &
      82, 82, 82 /)
  INTEGER, PARAMETER, DIMENSION(975) :: LU_IROW = (/&
    LU_IROW_0, LU_IROW_1, LU_IROW_2 /)

  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_0 = (/ &
       1,  2, 18, 80,  3, 37, 44, 45, 46, 50, 69, 79, &
       4, 44, 45, 52, 53, 54, 56, 57, 62, 65, 69, 70, &
      72, 76, 79, 81, 82,  5, 19, 20, 23, 42, 44, 45, &
      46, 69, 75, 79, 80,  6, 29,  7, 64,  8, 80,  9, &
      38, 74, 80, 10, 80, 11, 65, 77, 12, 78, 80, 13, &
      17, 80, 14, 80, 15, 75, 77, 16, 61, 74, 80, 17, &
      75, 80, 13, 17, 18, 75, 80, 19, 80, 20, 80, 21, &
      80, 22, 39, 80, 14, 19, 20, 23, 24, 42, 44, 45, &
      46, 69, 75, 79, 80, 19, 20, 24, 25, 42, 44, 45, &
      46, 69, 75, 79, 80, 19, 20, 25, 42, 44, 45, 69, &
      75, 79, 80,  1, 26, 40, 66, 79, 27, 74, 80, 81, &
      28, 61, 77, 80, 29, 74, 80, 82, 30, 45, 69, 79, &
      80, 19, 20, 31, 75, 80, 28, 32, 39, 61, 77, 78, &
      80, 33, 74, 77, 80, 34, 77, 80, 82,  1, 26, 35, &
      40, 66, 75, 77, 79, 36, 42, 44, 46, 66, 74, 79, &
      80, 37, 75, 79, 80, 38, 46, 74, 78, 80, 38, 39, &
      46, 74, 75, 78, 80, 33, 35, 40, 66, 74, 75, 77, &
      78, 79, 80, 15, 31, 41, 48, 51, 63, 64, 71, 74, &
      75, 77, 80, 42, 75, 79, 80, 22, 37, 39, 42, 43, &
      44, 45, 46, 48, 50, 61, 63, 64, 65, 69, 71, 74, &
      75, 78, 79, 80, 81, 82, 44, 75, 79, 80, 44, 45, &
      75, 79, 80, 46, 75, 79, 80, 31, 47, 74, 75, 77, &
      80, 81, 82, 48, 59, 65, 75, 78, 80, 81, 82, 20, &
      49, 74, 78, 80, 81, 82,  9, 38, 44, 46, 50, 67, &
      74, 75, 78, 79, 80, 81, 82, 49, 51, 59, 74, 75, &
      78, 80, 81, 82, 10, 52, 74, 78, 80, 81, 82, 14, &
      53, 74, 78, 80, 81, 82, 54, 69, 74, 78, 80, 81, &
      82, 21, 42, 52, 53, 54, 55, 68, 69, 73, 74, 75, &
      76, 78, 79, 80, 81, 82, 37, 56, 74, 75, 78, 79, &
      80, 81, 82, 45, 57, 74, 75, 78, 79, 80, 81, 82 /)
  INTEGER, PARAMETER, DIMENSION(360) :: LU_ICOL_1 = (/ &
      10, 14, 31, 34, 58, 65, 74, 75, 77, 78, 80, 81, &
      82, 19, 59, 74, 78, 80, 81, 82, 47, 49, 52, 53, &
      54, 56, 57, 58, 59, 60, 65, 67, 68, 69, 70, 72, &
      74, 75, 76, 77, 78, 79, 80, 81, 82, 16, 28, 46, &
      50, 61, 67, 74, 75, 77, 78, 79, 80, 81, 82, 37, &
      42, 44, 45, 62, 69, 74, 75, 78, 79, 80, 81, 82, &
      32, 39, 46, 49, 50, 59, 61, 63, 65, 67, 70, 74, &
      75, 77, 78, 79, 80, 81, 82, 21, 22, 27, 34, 37, &
      38, 39, 44, 45, 46, 47, 48, 49, 52, 53, 54, 56, &
      57, 58, 59, 61, 62, 64, 65, 67, 68, 69, 70, 72, &
      74, 75, 76, 77, 78, 79, 80, 81, 82, 11, 31, 51, &
      59, 65, 74, 75, 77, 78, 80, 81, 82,  8, 10, 14, &
      21, 26, 29, 30, 33, 36, 40, 41, 42, 44, 45, 46, &
      48, 51, 55, 59, 63, 64, 65, 66, 67, 68, 69, 70, &
      71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, &
      44, 67, 74, 75, 78, 79, 80, 81, 82, 42, 68, 74, &
      75, 78, 79, 80, 81, 82, 67, 69, 74, 75, 78, 79, &
      80, 81, 82, 42, 44, 55, 68, 69, 70, 73, 74, 75, &
      76, 78, 79, 80, 81, 82, 21, 42, 45, 52, 53, 54, &
      56, 57, 60, 62, 65, 67, 68, 69, 70, 71, 72, 73, &
      74, 75, 76, 77, 78, 79, 80, 81, 82,  8, 42, 44, &
      55, 68, 69, 72, 73, 74, 75, 76, 78, 79, 80, 81, &
      82, 47, 52, 53, 62, 67, 68, 69, 73, 74, 75, 76, &
      77, 78, 79, 80, 81, 82, 18, 19, 20, 21, 27, 31, &
      32, 33, 36, 37, 38, 39, 42, 43, 44, 45, 46, 47, &
      48, 49, 50, 51, 52, 53, 54, 56, 57, 58, 59, 60, &
      61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, &
      73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 15, 17, &
      31, 34, 37, 41, 42, 44, 45, 46, 48, 51, 59, 63, &
      64, 65, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76 /)
  INTEGER, PARAMETER, DIMENSION(255) :: LU_ICOL_2 = (/ &
      77, 78, 79, 80, 81, 82, 21, 60, 65, 67, 68, 69, &
      70, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, &
      11, 15, 22, 28, 33, 34, 35, 38, 39, 40, 41, 46, &
      47, 48, 49, 51, 52, 53, 54, 56, 57, 58, 59, 61, &
      62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, &
      74, 75, 76, 77, 78, 79, 80, 81, 82, 12, 35, 38, &
      40, 46, 49, 52, 53, 54, 56, 57, 58, 59, 61, 62, &
      65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, &
      77, 78, 79, 80, 81, 82, 35, 37, 40, 42, 44, 45, &
      46, 50, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, &
      76, 77, 78, 79, 80, 81, 82,  8, 10, 12, 13, 14, &
      16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, &
      28, 29, 30, 31, 32, 33, 34, 36, 37, 39, 40, 41, &
      42, 43, 44, 45, 46, 48, 50, 51, 55, 59, 60, 61, &
      63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, &
      75, 76, 77, 78, 79, 80, 81, 82, 27, 29, 30, 45, &
      46, 47, 49, 52, 53, 54, 56, 57, 58, 59, 62, 65, &
      67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, &
      79, 80, 81, 82, 29, 34, 46, 47, 49, 50, 51, 52, &
      53, 54, 55, 56, 57, 58, 59, 61, 62, 63, 65, 67, &
      68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, &
      80, 81, 82 /)
  INTEGER, PARAMETER, DIMENSION(975) :: LU_ICOL = (/&
    LU_ICOL_0, LU_ICOL_1, LU_ICOL_2 /)

  INTEGER, PARAMETER, DIMENSION(83) :: LU_CROW = (/ &
       1,  2,  5, 13, 30, 42, 44, 46, 48, 52, 54, 57, &
      60, 63, 65, 68, 72, 75, 80, 82, 84, 86, 89,102, &
     114,124,129,133,137,141,146,151,158,162,166,174, &
     182,186,191,198,208,220,224,247,251,256,260,268, &
     276,283,296,305,312,319,326,343,352,361,374,381, &
     406,420,433,452,490,502,541,550,559,568,583,610, &
     626,643,695,727,745,790,823,848,909,941,976 /)

  INTEGER, PARAMETER, DIMENSION(83) :: LU_DIAG = (/ &
       1,  2,  5, 13, 30, 42, 44, 46, 48, 52, 54, 57, &
      60, 63, 65, 68, 72, 77, 80, 82, 84, 86, 92,104, &
     116,125,129,133,137,141,148,152,158,162,168,174, &
     182,186,192,200,210,220,228,247,252,256,261,268, &
     277,287,297,306,313,319,331,344,353,365,375,390, &
     410,424,440,474,494,524,542,551,560,573,598,616, &
     633,686,719,738,784,818,844,906,939,975,976 /)


END MODULE kpp_JacobianSP


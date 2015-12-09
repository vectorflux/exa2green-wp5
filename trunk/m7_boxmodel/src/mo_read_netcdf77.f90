MODULE mo_read_netcdf77
  !
  ! Description: 
  !
  ! subroutines for reading (hyperslabs) of variables from netcdf files
  ! order of dimensions in netcdf files and fortran variable can be different
  !
  ! Authors:
  !
  ! J.S. Rast, MPI, May 2005, change all netcdf routines to fortran 77 calls
  ! D.O'Donnell, MPI-M, Feb 2008, added read 4-D variable

  USE mo_kind,      ONLY: dp 
  USE mo_netcdf,    ONLY: nf_check, nf_max_var_dims, nf__open,       &
                          nf_close, chunksize, nf_nowrite,           &
                          nf_inq_dimlen, nf_inq_dimid, nf_inq_varid, &
                          nf_get_vara_double, nf_inq_varndims,       &
                          nf_inq_vardimid

  IMPLICIT NONE

  PRIVATE

  PUBLIC    :: read_var_nf77_1d          ! read 1D-variable 
  PUBLIC    :: read_var_nf77_4d          ! read 4D-variable 

CONTAINS
!===============================================================================
  SUBROUTINE read_var_nf77_1d (file_name, dimname, varname, varptr, ierr)

    ! Description:
    !
    ! read in from a netCDF file variable varname(dimname).
    !
    ! Method:
    ! 
    ! Open the netCDF file named file_name and read in varptr varname(dimname).
    ! The routine is to be intended to run on the I/O-processor
    ! 
    ! Parameter list:
    !
    ! CHARACTER (LEN = *), INTENT (in) :: file_name ! file name of the netCDF file
    !    from which variable should be read
    ! CHARACTER (LEN = *), INTENT (in) :: dimname ! name of dimension in the netCDF
    !    file which shall appear as the dimension of varptr.
    ! CHARACTER (LEN = *), INTENT (in) :: varname   ! name of variable to be read
    ! REAL(dp), INTENT(out)            :: varptr(:) ! variable values on exit, has to have
    !    dimension exactly as the lengths of dimname in netCDF file
    ! INTEGER, INTENT(out)             :: ierr ! ierr is 0 if variable varname is successfully 
    !    read, 1 otherwise
    !
    ! Authors:
    ! 
    ! J.S. Rast, MPI, August 2004, original source
    !
    ! Arguments:
    !
    CHARACTER (LEN = *), INTENT (in)      :: file_name
    CHARACTER (LEN = *), INTENT (in)      :: dimname
    CHARACTER (LEN = *), INTENT (in)      :: varname 
    REAL(dp), INTENT(out)                 :: varptr(:)
    INTEGER, INTENT (out)                 :: ierr
    !
    ! Local variables:
    !
    INTEGER                               :: zncid, zdimlength, znvar, zvardims
    INTEGER                               :: zdims, zdimid

    zdims = SIZE (varptr)
! open netCDF file
    CALL nf_check(nf__open(TRIM(file_name), nf_nowrite, chunksize, zncid), fname=TRIM(file_name))
! verify lenght of dimension
    CALL nf_check(nf_inq_dimid(zncid, TRIM(dimname), zdimid), fname=TRIM(file_name))
    CALL nf_check(nf_inq_dimlen(zncid, zdimid, zdimlength), fname=TRIM(file_name))
    IF (zdimlength /= zdims) THEN
!gf
!       CALL finish('read_var_nf77_1d:', 'wrong length of dim in file '//TRIM(file_name))
       WRITE(*,*) 'read_var_nf77_1d: wrong length of dim in file --> ' //TRIM(file_name)
!gf

    END IF
! inquire variable and number of dimensions
    ierr=nf_inq_varid(zncid, TRIM(varname), znvar)
    IF (ierr == 0) THEN
       CALL nf_check(nf_inq_varndims(zncid, znvar, zvardims), fname=TRIM(file_name))
       IF (zvardims /= 1) THEN
!gf
!          CALL finish('read_var_nf77_1d:', &
!               'wrong number of dimension of variable '//TRIM(varname))
          WRITE(*,*) 'read_var_nf77_1d:wrong number of dimension of variable --> ' //TRIM(varname)
!gf

       END IF
       CALL nf_check(nf_get_vara_double(zncid, znvar, (/1/), (/zdims/), varptr), fname=TRIM(file_name))
    ELSE
!gf
!       CALL finish('read_var_nf77_1d:', &
!               'variable '//TRIM(varname)//' not found in '//TRIM(file_name))
       WRITE(*,*) 'read_var_nf77_1d:variable --> '//TRIM(varname)//' not found in '//TRIM(file_name)
!gf
    END IF
  END SUBROUTINE read_var_nf77_1d
!===============================================================================
!===============================================================================
    SUBROUTINE read_var_nf77_4d (file_name, dimname1, dimname2, dimname3, dimname4, &
                               var_name, varptr, ierr)

    ! Description:
    !
    ! read in a variable from a netCDF file
    !
    ! Method:
    ! 
    ! Open the netCDF file named file_name and read in varptr the values of var_name
    ! dimname1,...,dimname4 are the names of the dimension in the order as they will
    ! be in varptr. The order of the dimensions in the netCDF file is irrelevant.
    ! The routine is to be intended to run on the I/O-processor
    ! 
    ! Parameter list:
    !
    ! CHARACTER (LEN = *), INTENT (in) :: file_name ! file name of the netCDF file
    !    from which variable should be read
    ! CHARACTER (LEN = *), INTENT (in) :: dimname1 ! name of dimension in the netCDF
    !    file which shall appear as the first dimension of varptr. var_name can 
    !    have this dimension at any of the three possible positions
    ! CHARACTER (LEN = *), INTENT (in) :: dimname2 ! name of dimension in the netCDF
    !    file which shall appear as the second dimension of varptr. var_name can 
    !    have this dimension at any of the three possible positions
    ! CHARACTER (LEN = *), INTENT (in) :: dimname3 ! name of dimension in the netCDF
    !    file which shall appear as the third dimension of varptr. var_name can 
    !    have this dimension at any of the three possible positions
    ! CHARACTER (LEN = *), INTENT (in) :: dimname4 ! name of dimension in the netCDF
    !    file which shall appear as the fourth dimension of varptr. var_name can 
    !    have this dimension at any of the three possible positions
    ! CHARACTER (LEN = *), INTENT (in) :: var_name ! name of the variable to be read
    ! REAL(dp), INTENT(out) :: varptr(:,:,:,:) ! variable values on exit, has to
    !    dimension exactly as the lengths of dimname1,...,dimname4 in netCDF file
    ! INTEGER, INTENT(out) :: ierr ! ierr is 0 if variable var_name is successfully 
    !    read, 1 if it was not found in file file_name
    !
    ! Authors:
    ! 
    ! D. O'Donnell, MPI-M, Feb 2008, based on other read_var_... subroutines by J.S.Rast
    !
    !
    ! Arguments:
    !
    
    CHARACTER (LEN = *), INTENT (in)      :: file_name
    CHARACTER (LEN = *), INTENT (in)      :: dimname1
    CHARACTER (LEN = *), INTENT (in)      :: dimname2
    CHARACTER (LEN = *), INTENT (in)      :: dimname3
    CHARACTER (LEN = *), INTENT (in)      :: dimname4
    CHARACTER (LEN = *), INTENT (in)      :: var_name
    REAL(dp), INTENT(out)                 :: varptr(:,:,:,:)
    INTEGER, INTENT (out)                 :: ierr
    !
    ! Local variables:
    !
    INTEGER                               :: zncid, zdimlength, zvarid, zvardims, i, j
    INTEGER, DIMENSION(4)                 :: zdims, zdimid, zorder, zcountvar
    INTEGER, DIMENSION(NF_MAX_VAR_DIMS)   :: zvardimids
    REAL(dp), POINTER                     :: zin(:,:,:,:)

! define dimension length vector
    zdims(1) = SIZE (varptr, DIM = 1)
    zdims(2) = SIZE (varptr, DIM = 2)
    zdims(3) = SIZE (varptr, DIM = 3)
    zdims(4) = SIZE (varptr, DIM = 4)
! open netCDF file
    CALL nf_check(nf__open(TRIM(file_name), nf_nowrite, chunksize, zncid), fname=TRIM(file_name))
! verify lenghts of dimensions
    CALL nf_check(nf_inq_dimid(zncid, TRIM(dimname1), zdimid(1)), fname=TRIM(file_name))
    CALL nf_check(nf_inq_dimlen(zncid, zdimid(1), zdimlength), fname=TRIM(file_name))
    IF (zdimlength /= zdims(1)) THEN
!gf
!       CALL finish('read_var_nf77_4d:', 'wrong length of dim1 in initial file '//TRIM(file_name))
       WRITE(*,*) 'read_var_nf77_4d:wrong length of dim1 in initial file --> ' //TRIM(file_name)
    END IF
!gf
    CALL nf_check(nf_inq_dimid(zncid, TRIM(dimname2), zdimid(2)), fname=TRIM(file_name))
    CALL nf_check(nf_inq_dimlen(zncid, zdimid(2), zdimlength), fname=TRIM(file_name))
    IF (zdimlength /= zdims(2)) THEN
!gf
!       CALL finish('read_var_nf77_4d:', 'wrong length of dim2 in initial file '//TRIM(file_name))
       WRITE(*,*) 'read_var_nf77_4d:wrong length of dim2 in initial file --> ' //TRIM(file_name)
!gf
    END IF
    CALL nf_check(nf_inq_dimid(zncid, TRIM(dimname3), zdimid(3)), fname=TRIM(file_name))
    CALL nf_check(nf_inq_dimlen(zncid, zdimid(3), zdimlength), fname=TRIM(file_name))
    IF (zdimlength /= zdims(3)) THEN
!gf
!       CALL finish('read_var_nf77_4d:', 'wrong length of dim3 in initial file '//TRIM(file_name))
       WRITE(*,*) 'read_var_nf77_4d:wrong length of dim3 in initial file --> ' //TRIM(file_name)
!gf
    END IF
    CALL nf_check(nf_inq_dimid(zncid, TRIM(dimname4), zdimid(4)), fname=TRIM(file_name))
    CALL nf_check(nf_inq_dimlen(zncid, zdimid(4), zdimlength), fname=TRIM(file_name))
    IF (zdimlength /= zdims(4)) THEN
!gf
!       CALL finish('read_var_nf77_4d:', 'wrong length of dim4 in initial file '//TRIM(file_name))
       WRITE(*,*) 'read_var_nf77_4d:wrong length of dim4 in initial file --> ' //TRIM(file_name)
!gf
    END IF
    ierr=nf_inq_varid(zncid, TRIM(var_name), zvarid)
    IF (ierr == 0) THEN
       CALL nf_check(nf_inq_varndims(zncid, zvarid, zvardims), fname=TRIM(file_name))
       CALL nf_check(nf_inq_vardimid(zncid, zvarid, zvardimids), fname=TRIM(file_name))
       IF (zvardims /= 4) THEN
!gf
!          CALL finish('read_var_nf77_4d:', &
!               'wrong number of dimension of variable '//TRIM(var_name))
          WRITE(*,*) 'read_var_nf77_4d:wrong number of dimension of variable --> ' //TRIM(var_name)
!gf
       END IF
       DO i = 1, zvardims
          DO j = 1, zvardims
             IF (zvardimids(j) == zdimid(i)) THEN
                zorder(i)=j
                EXIT
             END IF
          END DO
       END DO
       ALLOCATE(zin(zdims(zorder(1)), zdims(zorder(2)), zdims(zorder(3)), zdims(zorder(4)) ))
       zcountvar(1)=zdims(zorder(1))
       zcountvar(2)=zdims(zorder(2))
       zcountvar(3)=zdims(zorder(3))
       zcountvar(4)=zdims(zorder(4))
       CALL nf_check(nf_get_vara_double(zncid, zvarid, (/1,1,1,1/), zcountvar, zin), fname=TRIM(file_name))
       CALL nf_check(nf_close(zncid), fname=TRIM(file_name))
       IF (ALL(zorder == (/ 1, 2, 3, 4 /))) THEN
         varptr = zin         
       ELSE
         varptr = RESHAPE(zin, zdims, order=zorder)
       ENDIF
       IF (ASSOCIATED(zin)) DEALLOCATE(zin)
    ELSE
!gf
!       CALL finish('read_var_nf77_4d:', &
!               'variable '//TRIM(var_name)//' not found in '//TRIM(file_name))
       WRITE(*,*) 'read_var_nf77_4d:variable --> '//TRIM(var_name)//' not found in '//TRIM(file_name)

!gf
    END IF
  END SUBROUTINE read_var_nf77_4d
!===============================================================================
!===============================================================================
  END MODULE mo_read_netcdf77

MODULE mo_netcdf

  USE mo_kind,      ONLY: dp

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: nf_check           ! checks/print the status of netcdf operations
  PUBLIC :: nf_noerr           ! mo_io
  PUBLIC :: nf_nowrite         ! mo_io
  PUBLIC :: nf_max_var_dims    ! mo_read_netcdf
  PUBLIC :: nf_inq_varid       ! mo_io
  PUBLIC :: nf_inq_dimid       ! mo_io
  PUBLIC :: nf_inq_dimlen      !
  PUBLIC :: nf_get_var_double  !
  PUBLIC :: nf_get_vara_double !
  PUBLIC :: nf_open            ! mo_io
  PUBLIC :: nf__open           ! mo_io, version allows setting I/O buffer size
  PUBLIC :: nf_close           ! mo_io
  PUBLIC :: nf_inq_vardimid    ! mo_read_ncdf_f77
  PUBLIC :: nf_inq_varndims
  PUBLIC :: nf_get_var1_double
  PUBLIC :: io_inq_dimid       ! 
  PUBLIC :: io_inq_dimlen      !
  PUBLIC :: io_inq_varid       !
  PUBLIC :: chunksize          ! preferred netCDF I/O buffer size 

  INCLUDE 'netcdf.inc'

  !-----------------------------------------------------------------------------
  ! 
  ! Due to I/O performance problems, means insufficient return values by
  ! OS information in the stat() system call, we change the buffer size 
  ! to 16 MB fuer netcdf I/O buffer, cannot be PARAMETER, because the netCDF
  ! library is written in C and we get trouble passing parameters.

#if defined (__SX__) || defined (ES) || defined (__PGI)
  INTEGER :: initialsize = 33554432      ! that's 32 MByte   
  INTEGER :: chunksize   = 33554432      ! too
#else
  INTEGER :: initialsize =    32768      ! that's 32 kByte   
  INTEGER :: chunksize   =    32768      ! too
#endif

  !-----------------------------------------------------------------------------
  TYPE FILE_INFO
    
    LOGICAL :: opened                       ! open = .true. or closed = .false.
    LOGICAL :: parallel                     ! parallel .true. else .false.    
    INTEGER :: file_id                      ! netCDF file id 
    INTEGER :: access_mode                  ! access mode for that file
    INTEGER :: ncdims(NF_MAX_VAR_DIMS) 
    INTEGER :: FORMAT                       ! file format NETCDF
    
    CHARACTER(len=NF_MAX_NAME) :: creation_program ! name of this program
    CHARACTER(len=NF_MAX_NAME) :: creation_user    ! who has run this program
    CHARACTER(len=NF_MAX_NAME) :: creation_date    ! created netCDF file at
    CHARACTER(len=NF_MAX_NAME) :: binary_source    ! binary type (CRAY or IEEE)
    CHARACTER(len=NF_MAX_NAME) :: file_type        ! initital or restart file
    CHARACTER(len=NF_MAX_NAME) :: file_name        ! nc file name
    CHARACTER(len=NF_MAX_NAME) :: title
  END TYPE FILE_INFO
  !-----------------------------------------------------------------------------
  INTEGER, PARAMETER :: max_dim_name = 32
  !-----------------------------------------------------------------------------
  TYPE IO_dim
    INTEGER                     :: dim_id   =   0      ! temporary NetCDF id
    INTEGER                     :: var_id   =   0      ! temporary NetCDF id
    INTEGER                     :: dim_len  =  -1
    CHARACTER(len=max_dim_name) :: dim_name =  ''
    CHARACTER(len=64)           :: longname =  ''
    CHARACTER(len=32)           :: units    =  ''
    INTEGER                     :: levtyp   =   0      ! GRIB level type
    LOGICAL                     :: single   =  .FALSE. ! single level
    REAL(dp)  ,POINTER          :: VALUE(:) => NULL()
  END TYPE IO_dim
  !-----------------------------------------------------------------------------
  INTEGER       ,PARAMETER    :: max_dim_ids = 50
  INTEGER                     :: IO_ndim_ids
  TYPE (IO_dim) ,TARGET ,SAVE :: IO_dim_ids (max_dim_ids)

  !-----------------------------------------------------------------------------
  ! data type to hold file attributes
  !----------------------------------
  TYPE t_att_text
    CHARACTER(len= 32) :: name = ''
    CHARACTER(len=128) :: text = ''
  END TYPE t_att_text

  TYPE (t_att_text) ,SAVE :: global_att (20)
  !-----------------------------------------------------------------------------
CONTAINS

  !-----------------------------------------------------------------------------
  SUBROUTINE IO_INQ_DIMID (ncid, name, dimid)
    INTEGER :: ncid, dimid, status
    CHARACTER(len=*) :: name

    status = NF_INQ_DIMID (ncid, name, dimid)
    IF (status /= NF_NOERR) THEN
!gf
!      CALL message ('IO_INQ_DIMID', name)
!      CALL message ('IO_INQ_DIMID', NF_STRERROR(status))
!      CALL finish  ('IO_INQ_DIMID', 'Run terminated.')
      WRITE(*,*) 'IO_INQ_DIMID --> ', name
      WRITE(*,*) 'IO_INQ_DIMID --> ', NF_STRERROR(status)
      STOP
!gf
    END IF

  END SUBROUTINE IO_INQ_DIMID
  !-----------------------------------------------------------------------------
  SUBROUTINE IO_INQ_DIMLEN (ncid, dimid, len)
    INTEGER :: ncid, dimid, len, status

    status = NF_INQ_DIMLEN (ncid, dimid, len)
    IF (status /= NF_NOERR) THEN
!gf
!      WRITE(message_text,*) ncid, dimid
!      CALL message ('IO_INQ_DIMLEN', NF_STRERROR(status))
!      CALL finish  ('IO_INQ_DIMLEN', 'Run terminated.')
      WRITE(*,*) ncid, dimid
      WRITE(*,*) 'IO_INQ_DIMLEN --> ', NF_STRERROR(status)
      STOP
!gf
    END IF

  END SUBROUTINE IO_INQ_DIMLEN
  !-----------------------------------------------------------------------------
  SUBROUTINE IO_INQ_VARID (ncid, name, varid)
    INTEGER :: ncid, varid, status
    CHARACTER(len=*) :: name

    status = NF_INQ_VARID (ncid, name, varid)
    IF (status /= NF_NOERR) THEN
!gf
!      WRITE(message_text,*) ncid, name
!      CALL message ('IO_INQ_VARID', message_text)
!      CALL message ('IO_INQ_VARID', NF_STRERROR(status))
!      CALL finish  ('IO_INQ_VARID', 'Run terminated.')
      WRITE(*,*) ncid, name
      WRITE(*,*) 'IO_INQ_VARID ---> ', NF_STRERROR(status)
      STOP
!gf
    END IF

  END SUBROUTINE IO_INQ_VARID
  !-----------------------------------------------------------------------------
  ! turns nf_* function into subroutine and checks status
  SUBROUTINE nf_check(status, fname) 
    INTEGER :: status
    CHARACTER(len=*), OPTIONAL :: fname

    IF (status /= nf_noerr) THEN
      IF (PRESENT(fname)) THEN
!gf        CALL finish('netcdf error in '//TRIM(fname),nf_strerror(status))
        WRITE(*,*) 'netcdf error in --> ' //TRIM(fname),nf_strerror(status)
      ELSE
!gf        CALL finish('netcdf error',nf_strerror(status))
        WRITE(*,*) 'netcdf error --> ',nf_strerror(status)
      END IF
    ENDIF

  END SUBROUTINE nf_check
  !-----------------------------------------------------------------------------
END MODULE mo_netcdf

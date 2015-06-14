MODULE Wrap_NETCDF

  USE netcdf 
  USE box_model_driver_parameters, ONLY: idim_spot, jdim_spot, kdim_spot

  IMPLICIT NONE
  PUBLIC
  SAVE
  INTEGER, PARAMETER :: idim = 222, jdim = 216, kdim = 40 ! global parameters
  INTEGER, PARAMETER :: n_from_file  = 57 ! number of species with initial values read from *nc file
  INTEGER, PARAMETER :: nfix  =  5 ! number of species with fixed, non-zero initial values
  INTEGER, PARAMETER :: nradicals = 21 ! number of radical species
  INTEGER, PARAMETER :: ntot = 83
  CHARACTER(10), PARAMETER :: VAR_NAMES(ntot)=(/ &
       ! species with initial values read from the *nc files
       "CO        ", "NO2       ", "OZONE     ", "H2O2      ", "HNO3      ", "NH3       ", "NO        ", "SO2       ", &
       "HCHO      ", "ETH       ", "ISO       ", "HO2       ", "OH        ", "SULF      ", "ALD       ", "OP1       ", &
       "OP2       ", "PAA       ", "ORA1      ", "ORA2      ", "N2O5      ", "NO3       ", "PAN       ", "HC3       ", &
       "HC5       ", "HC8       ", "OL2       ", "OLT       ", "OLI       ", "TOL       ", "XYL       ", "ACO3      ", &
       "TPAN      ", "HONO      ", "HNO4      ", "KET       ", "GLY       ", "MGLY      ", "DCB       ", "ONIT      ", &
       "CSL       ", "API       ", "LIM       ", "HACE      ", "ISHP      ", "ISON      ", "MACR      ", "MAHP      ", &
       "MPAN      ", "NALD      ", "SOA1      ", "SOA10     ", "SOA100    ", "SOA1000   ", "DMS       ", "DMSO      ", & 
       ! Extra treatment for H2O
       "QV        ", &
       ! species with fixed, non-zero initial values
       "CO2       ", "H2        ", "O2        ", "N2        ", "CH4       ", &
       !  radical species 
       "MO2       ", "OLN       ", "MACP      ", "ISOP      ", "O1D       ", "O3P       ", "TCO3      ", "HC3P      ", &
       "HC5P      ", "HC8P      ", "OL2P      ", "OLTP      ", "OLIP      ", "TOLP      ", "XYLP      ", "ETHP      ", &
       "KETP      ", "XO2       ", "XNO2      ", "APIP      ", "LIMP      "/)
  INTEGER, PARAMETER :: lookup(n_from_file)=(/ &
       ! lookup table for the variable species read from the *nc files and being updated at every hour in data_kpp
       42,75,78,36,45, 1,77,16, &
       64, 9,46,73,82, 3,71,30, &
       60,28, 4, 5,17,80,34,22, &
       11,15,37,44,69,20,21,81, &
       12,13,33,55,47,63,51,70, &
       31,41,43,32,10,39,50,18, & 
       29,23, 6,24,25,26,19,14, &
       66 /)

CONTAINS
  
  SUBROUTINE ShuffleFromFile2KPP(V, V_from_file)
    IMPLICIT NONE

    REAL, INTENT(out) :: V(ntot) ! V - Concentrations of variable species (local)
    REAL, INTENT(in)  :: V_from_file(ntot) ! V_USER - Concentration of variable species in USER's order
    
    V(1)  = V_from_file(6 ) !NH3
    V(2)  = V_from_file(61) !N2
    V(3)  = V_from_file(14) !SULF
    V(4)  = V_from_file(19) !ORA1
    V(5)  = V_from_file(20) !ORA2
    V(6)  = V_from_file(51) !CS1
    V(7)  = V_from_file(58) !CO2
    V(8)  = V_from_file(59) !H2
    V(9)  = V_from_file(10) !ETH
    V(10) = V_from_file(45) !ISHP
    V(11) = V_from_file(25) !HC5
    V(12) = V_from_file(33) !TPAN
    V(13) = V_from_file(34) !HONO 
    V(14) = V_from_file(56) !DMSO
    V(15) = V_from_file(26) !HC8
    V(16) = V_from_file(8 ) !SO2
    V(17) = V_from_file(21) !N2O5
    V(18) = V_from_file(48) !MAHP
    V(19) = V_from_file(55) !DMS
    V(20) = V_from_file(30) !TOL
    V(21) = V_from_file(31) !XYL
    V(22) = V_from_file(24) !HC3
    V(23) = V_from_file(50) !NALD
    V(24) = V_from_file(52) !CS10
    V(25) = V_from_file(53) !CS100
    V(26) = V_from_file(54) !CS1000
    V(27) = V_from_file(66) !O1D
    V(28) = V_from_file(18) !PAA
    V(29) = V_from_file(49) !MPAN
    V(30) = V_from_file(16) !OP1
    V(31) = V_from_file(41) !CSL
    V(32) = V_from_file(44) !HACE
    V(33) = V_from_file(35) !HNO4
    V(34) = V_from_file(23) !PAN
    V(35) = V_from_file(68) !O3P
    V(36) = V_from_file(4 ) !H2O2
    V(37) = V_from_file(27) !OL2
    V(38) = V_from_file(66) !ISOP
    V(39) = V_from_file(46) !ISON
    V(40) = V_from_file(60) !O2
    V(41) = V_from_file(42) !API
    V(42) = V_from_file(1 ) !CO
    V(43) = V_from_file(43) !LIM
    V(44) = V_from_file(28) !OLT
    V(45) = V_from_file(5 ) !HNO3
    V(46) = V_from_file(11) !ISO
    V(47) = V_from_file(37) !GLY
    V(48) = V_from_file(81) !XNO2
    V(49) = V_from_file(77) !XYLP
    V(50) = V_from_file(47) !MACR
    V(51) = V_from_file(39) !DCB
    V(52) = V_from_file(71) !HC5P
    V(53) = V_from_file(72) !HC8P
    V(54) = V_from_file(75) !OLIP
    V(55) = V_from_file(36) !KET
    V(56) = V_from_file(73) !OL2P
    V(57) = V_from_file(74) !OLTP
    V(58) = V_from_file(80) !XO2
    V(59) = V_from_file(76) !TOLP
    V(60) = V_from_file(17) !OP2
    V(61) = V_from_file(65) !MACP
    V(62) = V_from_file(64) !OLN
    V(63) = V_from_file(38) !MGLY
    V(64) = V_from_file(9 ) !HCHO
    V(65) = V_from_file(69) !TCO3
    V(66) = V_from_file(57) !H2O
    V(67) = V_from_file(83) !LIMP
    V(68) = V_from_file(82) !APIP
    V(69) = V_from_file(29) !OLI
    V(70) = V_from_file(40) !ONIT
    V(71) = V_from_file(15) !ALD
    V(72) = V_from_file(78) !ETHP
    V(73) = V_from_file(12) !HO2
    V(74) = V_from_file(70) !HC3P
    V(75) = V_from_file(2 ) !NO2
    V(76) = V_from_file(63) !MO2
    V(77) = V_from_file(7 ) !NO
    V(78) = V_from_file(3 ) !O3
    V(79) = V_from_file(79) !KETP
    V(80) = V_from_file(22) !NO3
    V(81) = V_from_file(32) !ACO3
    V(82) = V_from_file(13) !HO
    V(83) = V_from_file(62) !CH4 
  END SUBROUTINE ShuffleFromFile2KPP

  SUBROUTINE ReadDataNETCDF(rbuf_var, rbuf_T, act_hour)
    IMPLICIT NONE

    REAL, INTENT(out) :: rbuf_var(idim, jdim, kdim, n_from_file)
    REAL, intent(out) :: rbuf_T(idim, jdim, kdim)

    CHARACTER (len = 28) :: FILE_NAME
    CHARACTER (len = 2):: act_hour_str
    INTEGER :: act_hour

    INTEGER :: iedims , jedims , kedims ! local indices
    INTEGER :: ncid, varid
    
    ! Loop indexes, and error handling.
    INTEGER :: var_cnt
    INTEGER :: ndimids
    INTEGER :: dimsids(4)
    
    REAL :: rbuf_(idim, jdim, kdim)

    WRITE(act_hour_str, '(I2.2)') act_hour
    FILE_NAME ="initial_data/lfff00"//TRIM(act_hour_str)//"0000.nc"
        
    WRITE(*,*) FILE_NAME

    rbuf_var(:,:,:,:) = 0.0
    rbuf_T(:,:,:) = 0.0
    
    ! Open file
    CALL check(nf90_open(FILE_NAME, NF90_NOWRITE, ncid) )
    
    ! Read Concentrations from *nc file for variables 1:n_from_file
    DO var_cnt = 1, n_from_file
       dimsids(:) = 0
       varid = 0
       rbuf_(:,:,:) = 0.0

       ! Get varid of the data available for species with name = VAR_NAMES(var_cnt)
       ! WRITE(*,*) "Read ", TRIM(VAR_NAMES(var_cnt))
       CALL check(nf90_inq_varid(ncid, TRIM(VAR_NAMES(var_cnt)), varid) )

       ! get the values of the dimension ids: iedims, jedims, klev
       CALL check(NF90_inquire_variable  (ncid, varid, ndims = ndimids, dimids = dimsids))
       CALL check(NF90_inquire_dimension (ncid, dimsids(1), len=iedims))
       CALL check(NF90_inquire_dimension (ncid, dimsids(2), len=jedims))
       CALL check(NF90_inquire_dimension (ncid, dimsids(3), len=kedims))
       
       CALL check(NF90_get_var (ncid, varid, rbuf_, &
            start=(/1,1,1,1/), count=(/iedims, jedims, kedims, 1/)))

       rbuf_var(1:iedims, 1:jedims, 1:kedims, var_cnt) = rbuf_
       
       !   WRITE(*,*) VAR_NAMES(var_cnt), rbuf_var(100,100,31,var_cnt)
    END DO

    varid = 0
    dimsids(:) = 0

    ! read temperature
    CALL check(nf90_inq_varid(ncid, "T", varid) )

    ! get the values of the dimension ids: iedims, jedims, klev
    CALL check(NF90_inquire_variable  (ncid, varid, ndims = ndimids, dimids = dimsids))
    CALL check(NF90_inquire_dimension (ncid, dimsids(1), len=iedims))
    CALL check(NF90_inquire_dimension (ncid, dimsids(2), len=jedims))
    CALL check(NF90_inquire_dimension (ncid, dimsids(3), len=kedims))
       
    CALL check(NF90_get_var (ncid, varid, rbuf_T,     &
            start=(/1,1,1,1/), count=(/iedims, jedims, kedims, 1/)))

    ! Close the file, freeing all resources.
    CALL check( nf90_close(ncid) )

    ! adjust QV to H2O
    rbuf_var(1:iedims, 1:jedims, 1:kedims, 57) = rbuf_var(1:iedims, 1:jedims, 1:kedims, 57)*1.604e6
END SUBROUTINE ReadDataNETCDF

SUBROUTINE GetFixValues(data_fix)
  IMPLICIT NONE

  REAL, INTENT(out) :: data_fix(idim, jdim, kdim, nfix)

  REAL, PARAMETER :: val(nfix) = (/372.9, 500.0, 20.9E+4, 78.1E+4, 1.8/) ! CO2, H2, O2, N2, H2O, CH4

  INTEGER :: var_cnt

  ! initialize fixed species
  DO var_cnt = 1, nfix
     data_fix(:, :, :, var_cnt) = val(var_cnt)
  END DO
END SUBROUTINE GetFixValues


SUBROUTINE GetRadicals(data_radicals)
  IMPLICIT NONE

  REAL, INTENT(out) :: data_radicals(idim, jdim, kdim, nradicals)

  ! initialize radicals
  data_radicals(:,:,:,:) = 0.0
  data_radicals(:,:,:,1) = 1.E-17
  data_radicals(:,:,:,2) = 1.E-17
  data_radicals(:,:,:,3) = 1.E-20
  data_radicals(:,:,:,4) = 1.E-20
  data_radicals(:,:,:,5) = 1.E-20
  data_radicals(:,:,:,6) = 1.E-20
END SUBROUTINE GetRadicals


SUBROUTINE Get_Boxes(data_kpp, local_temperature, act_hour)
  IMPLICIT NONE

  INTEGER, INTENT(in) :: act_hour
  REAL, INTENT(out) :: data_kpp(idim,jdim,kdim,ntot)
  REAL, INTENT(out) :: local_temperature(idim, jdim, kdim)

  REAL :: data_var_netcdf(idim, jdim, kdim, n_from_file)
  REAL :: data_fix(idim, jdim, kdim, nfix)
  REAL :: data_radicals(idim, jdim, kdim, nradicals)
  REAL :: data_all(idim, jdim, kdim, ntot) 

  REAL :: V_from_file(ntot)
  REAL :: V_kpp(ntot)

  INTEGER :: i, j, k, var_cnt

  CALL ReadDataNETCDF(data_var_netcdf, local_temperature, act_hour)

  ! Print variable concentrations at the monitoring point
  !CALL print_values_on_box(idim_spot, jdim_spot, kdim_spot, data_var_netcdf, act_hour)

  IF (act_hour == 0) THEN
     CALL GetFixValues(data_fix)
     CALL GetRadicals(data_radicals)
  ENDIF

  data_all(:,:,:,:) = 0.0

  !$OMP PARALLEL DEFAULT(SHARED) &
  !$OMP PRIVATE(k,j,i,var_cnt)
  !$OMP DO COLLAPSE(2)
  DO k=1,kdim
     DO j=1,jdim
        DO i=1,idim
           DO var_cnt=1,n_from_file
              data_all(i,j,k,var_cnt) = data_var_netcdf(i,j,k,var_cnt)
           END DO
        END DO
     END DO
  END DO
  !$OMP END DO 
  !$OMP END PARALLEL

  IF (act_hour == 0) THEN
     !$OMP PARALLEL DEFAULT(SHARED) &
     !$OMP PRIVATE(k,j,i,var_cnt)
     !$OMP DO COLLAPSE(2)
     DO k=1,kdim
        DO j=1,jdim
           DO i=1,idim
              DO var_cnt=1,nfix
                 data_all(i,j,k,var_cnt+n_from_file) = data_fix(i,j,k,var_cnt)
              END DO
           END DO
        END DO
     END DO
     !$OMP END DO 
     !$OMP END PARALLEL

     !$OMP PARALLEL DEFAULT(SHARED) &
     !$OMP PRIVATE(k,j,i,var_cnt)
     !$OMP DO COLLAPSE(2)
     DO k=1,kdim
        DO j=1,jdim
           DO i=1,idim
              DO var_cnt=1,nradicals
                 data_all(i,j,k,var_cnt+n_from_file+nfix) = data_radicals(i,j,k,var_cnt)
              END DO
           END DO
        END DO
     END DO
     !$OMP END DO 
     !$OMP END PARALLEL
  ENDIF

  IF (act_hour == 0) THEN
     DO k=1,kdim
        DO j=1,jdim
           DO i=1,idim
              V_from_file(1:ntot) = data_all(i,j,k,1:ntot)
              CALL ShuffleFromFile2KPP(V_kpp, V_from_file)
              data_kpp(i,j,k,1:ntot) = V_kpp
           END DO
        END DO
     END DO
  ELSE
     DO k=1,kdim
        DO j=1,jdim
           DO i=1,idim
              V_from_file(1:n_from_file) = data_all(i,j,k,1:n_from_file)

              CALL ShuffleFromFile2KPP(V_kpp, V_from_file)

              DO var_cnt=1,n_from_file-1
                 data_kpp(i,j,k,lookup(var_cnt)) = V_kpp(lookup(var_cnt))
              ENDDO
           END DO
        END DO
     END DO
  ENDIF

END SUBROUTINE Get_Boxes


SUBROUTINE print_values_on_box(i,j,k,data_var,act_hour)
  IMPLICIT NONE

  REAL    :: data_var(idim, jdim, kdim, n_from_file)
  INTEGER :: i, j, k, var_cnt, act_hour

  WRITE(*, *) "act_hour = ",act_hour,", Boxids: i = ",i, ", j = ",j, ", k = ",k
  DO var_cnt = 1, n_from_file
     WRITE(*,*) VAR_NAMES(var_cnt), data_var(i,j,k,var_cnt)
  END DO
END SUBROUTINE print_values_on_box


SUBROUTINE check(status)
  IMPLICIT NONE

  INTEGER, INTENT (in) :: status
    
  IF(status /= nf90_noerr) THEN 
     PRINT *, TRIM(nf90_strerror(status))
     STOP "Stopped"
  END IF
END SUBROUTINE check

END MODULE Wrap_NETCDF

dnl Process this file with autoconf to produce a configure script.
dnl
dnl Luis Kornblueh
dnl Uwe Schulzweida
dnl
dnl  - add cross compiling for NEC (use --host=sx6)
dnl  - use autoconf version 2.59 
dnl  - add cross compiling for Earth Simulator, Yokohama (use --host=es)
dnl  - added cross compiling for CRAY X1E
dnl  - added cross compiling for CRAY XT3
dnl

AC_INIT(echam,6.0.00,luis.kornblueh@zmaw.de,echam-6.0.00)
AC_PREREQ(2.59)

dnl Checks for package options.
dnl ######################################################################

AC_PREFIX_DEFAULT(.)

AC_CONFIG_AUX_DIR(config)

AC_CANONICAL_SYSTEM

dnl ######################################################################

AC_ARG_WITH([fortran], 
  [AS_HELP_STRING([--with-fortran=COMPILER],
    [select_available compiler (see README.compiler)])],
  [],
  [])

if test x$with_fortran != x; then
    fortran_compiler=$with_fortran      
else
    fortran_compiler="default"  
fi

case $host in
  i*86-*-linux-*)  
    if test x$fortran_compiler != xintel ; then
      gcc_lib="$(gcc -print-libgcc-file-name)"
    fi
    ;;
  *)               
    gcc_lib=""                               
    ;;
esac

dnl ######################################################################

AC_ARG_WITH([openmp],
  [AS_HELP_STRING([--with-openmp],
    [enable OpenMP, if available for selected compiler])],,
    withval=no)

if test "x$withval" == "xno" ; then
    use_openmp=$withval
else
    use_openmp="yes"
fi

AC_ARG_WITH([mpi],
            [AC_HELP_STRING([--with-mpi=DIR],
                            [MPI library])],,
                            withval=yes)

if test "x$withval" = "xno" ; then
    use_mpi=no
else
    use_mpi=yes
    if test "x$withval" != "xyes" ; then        
         use_MPIROOT=$withval
    fi
fi

dnl ######################################################################

ac_hostname=$(hostname)
ac_hostname=${ac_hostname%%\.*}

if [[ $(hostname) = $ac_hostname ]] ; then 
   fqdn=$(host -n $ac_hostname | awk '/has address/ {print $1}')
else
   fqdn=$(hostname)
fi
ac_sitename=$(echo $fqdn | awk -F. '{i=NF-1; printf("%s.%s",$i,$NF);}')

dnl ######################################################################

host_frag=""
case "${host}" in
    xt?-cray-*)          host_frag=config/mh-cray-xt3_untested ;;
    *-*-solaris2*)       host_frag=config/mh-solaris_untested ;;
    sx*-*-*)             host_frag=config/mh-nec_untested ;;
    *-ibm-aix*)          host_frag=config/mh-aix ;;
    x86_64-*-linux-*)    host_frag=config/mh-linux-x64 ;;  
    i*86-*-linux-*)      host_frag=config/mh-linux-ia32 ;;
    *-*-darwin*)         host_frag=config/mh-darwin ;;
    *) echo This configuration is not supported.
       echo Please set a valid config/mh-* file up.
       ;;               
esac

if test ! -f $host_frag ; then
   echo "machine dependent configuration file $host_frag does not exist!"
   exit 1
fi

if test "$build" != "$host" ; then
   CROSS=yes
else
   if test "$ARCH" = "CRAY_XT3" ; then
      CROSS=yes
   else
      CROSS=""
   fi
fi

if test -z "$CROSS" ; then
AC_CONFIG_HEADER(config/config.h)
fi

AC_GET_MH($host_frag)

reduced_CFLAGS=`echo $CFLAGS | sed 's/-g/ /'`
if test "x$CFLAGS" != "x$reduced_CFLAGS" ; then
   ac_cflags_debug_option="-g"
   CFLAGS=$reduced_CFLAGS
else
   ac_cflags_debug_option=""
fi

if test -z "${OMPFLAG}" ; then
  use_openmp="not available"
else
  if test "x${use_openmp}" == "xyes" ; then
    use_openmp="enabled"
  else
    use_openmp="not enabled"
  fi 
fi

FC=${F90:+$F90}

dnl ######################################################################
dnl Checks for programs.
dnl ######################################################################

AC_PATH_PROG(PERL, perl, /bin/false, \
                $PATH:$prefix/bin:/usr/bin:/usr/local/bin)
AC_PATH_PROG(GMAKE, gmake, /bin/false, \
                $PATH:$prefix/bin:/usr/bin:/usr/local/bin)

dnl ######################################################################

package_library=$LIBS
LIBS=""

if test -z "$CROSS" ; then
  AC_PROG_CC
  AC_PROG_FC([ifort nagfor pgfortran xlf95_r sunf95 gfortran])  
fi

dnl ######################################################################
dnl Checks for zlib.
dnl ######################################################################

ZLIB_LIB=""
ZLIB_INCLUDE=""

AC_ARG_WITH([zlib],
            [AC_HELP_STRING([--with-zlib=DIR],
                            [Required I/O package])],,
                            withval=yes)

if test x$withval = xyes; then
  if test x$with_zlib != x; then
    ZLIBROOT=$with_zlib
  fi
fi

if test -n "$ZLIBROOT" ; then
  AC_CHECK_FILE($ZLIBROOT/include/zlib.h,[],AC_MSG_ERROR([ZLIBROOT not properly defined]))      
  if test -r "$ZLIBROOT/lib/libz.a" ; then
     ZLIB_LIB='-L$(ZLIBROOT)/lib -lz'
  else
     echo "cannot read $ZLIBROOT/lib/libz.a"  
     exit 1    
  fi
  if test -d "$ZLIBROOT/include" ; then
     ZLIB_INCLUDE='-I$(ZLIBROOT)/include'
  else
     echo "cannot find $ZLIBROOT/include"  
     exit 1    
  fi

else
  echo "error: the variable ZLIBROOT is not set in $host_frag"  
  echo "       set ZLIBROOT to the zlib root directory"
  exit 1
fi

dnl ######################################################################
dnl Checks for SZIP.
dnl ######################################################################

SZIP_LIB=""
SZIP_INCLUDE=""

AC_ARG_WITH([szip],
            [AC_HELP_STRING([--with-szip=DIR],
                            [Required I/O package])],,
                            withval=yes)

if test x$withval = xyes; then
  if test x$with_szip != x; then
    SZIPROOT=$with_szip
  fi
fi

if test -n "$SZIPROOT" ; then
  AC_CHECK_FILE($SZIPROOT/include/szlib.h,[],AC_MSG_ERROR([SZIPROOT not properly defined]))     
  if test -r "$SZIPROOT/lib/libsz.a" ; then
     SZIP_LIB='-L$(SZIPROOT)/lib -lsz'
  else
     echo "cannot read $SZIPROOT/lib/libsz.a"  
     exit 1    
  fi
  if test -d "$SZIPROOT/include" ; then
     SZIP_INCLUDE='-I$(SZIPROOT)/include'
  else
     echo "cannot find $SZIPROOT/include"  
     exit 1    
  fi
else
  echo "warning: the variable SZIPROOT is not set in $host_frag"  
  echo "         Cannot determine path to szip library."
fi

dnl ######################################################################
dnl Checks for HDF5.
dnl ######################################################################

HDF5_LIB=""
HDF5_INCLUDE=""

AC_ARG_WITH([hdf5],
            [AC_HELP_STRING([--with-hdf5=DIR],
                            [Required I/O package])],,
                            withval=yes)

if test x$withval = xyes; then
  if test x$with_hdf5 != x; then
    HDF5ROOT=$with_hdf5
  fi
fi

if test -n "$HDF5ROOT" ; then
  AC_CHECK_FILE($HDF5ROOT/include/hdf5.h,[],AC_MSG_ERROR([HDF5ROOT not properly defined]))      
  if test -r "$HDF5ROOT/lib/libhdf5.a" ; then
     HDF5_LIB='-L$(HDF5ROOT)/lib -lhdf5_hl -lhdf5'
  else
     echo "cannot read $HDF5ROOT/lib/libhdf5.a"  
     exit 1    
  fi
  if test -d "$HDF5ROOT/include" ; then
     HDF5_INCLUDE='-I$(HDF5ROOT)/include'
  else
     echo "cannot find $HDF5ROOT/include"  
     exit 1    
  fi
else
  echo "error: the variable HDF5ROOT is not set in $host_frag"  
  echo "       set HDF5ROOT to the hdf5 root directory"
  exit 1
fi

dnl ######################################################################
dnl Checks for NETCDF.
dnl ######################################################################

NETCDF_LIB=""
NETCDF_INCLUDE=""

AC_ARG_WITH([netcdf],
            [AC_HELP_STRING([--with-netcdf=DIR],
                            [Required I/O package])],,
                            withval=yes)

if test x$withval = xyes; then
  if test x$with_netcdf != x; then
    NETCDFROOT=$with_netcdf
  fi
fi

AC_CHECK_FILE($NETCDFROOT/include/netcdf.inc,[],AC_MSG_ERROR([NETCDFROOT not properly defined]))
if test -n "$NETCDFROOT" ; then
  if test -r "$NETCDFROOT/lib/libnetcdff.a" ; then
     NETCDF_LIB='-L$(NETCDFROOT)/lib -lnetcdff -lnetcdf'
  elif test -r "$NETCDFROOT/lib/libnetcdf.a" ; then
     NETCDF_LIB='-L$(NETCDFROOT)/lib -lnetcdf'
  else
     echo "cannot read $NETCDFROOT/lib/libnetcdf.a"  
     exit 1    
  fi
  if test -r "$NETCDFROOT/include/netcdf.inc" ; then
     NETCDF_INCLUDE='-I$(NETCDFROOT)/include'
  else
     echo "cannot read $NETCDFROOT/include/netcdf.inc"
     exit 1
  fi
else
  echo "error: the variable NETCDFROOT is not set in $host_frag"  
  echo "       set NETCDFROOT to the netCDF root directory"
  exit 1
fi

dnl ######################################################################
dnl Checks for cdilib.
dnl ######################################################################

CDIROOT=$(pwd)/cdi
CDI_LIB=""
CDI_INCLUDE=""

AC_ARG_WITH([cdi],
            [AC_HELP_STRING([--with-cdi=DIR],
                            [Required I/O package, use internal, if not set])],,
                            withval=no)

if test x$withval = xyes; then
  if test x$with_cdi != x; then
    CDIROOT=$with_cdi
  fi
else
    SRCDIRS="${CDIROOT}/src ${SRCDIRS}"
fi

AC_CHECK_FILE($CDIROOT/include/cdi.inc,[],AC_MSG_ERROR([CDIROOT not properly defined])) 
if test -r "$CDIROOT/lib" ; then
   CDI_LIB='-L$(CDIROOT)/lib -lcdi'
else
   echo "cannot find $CDIROOT/lib"  
   exit 1    
fi
if test -r "$CDIROOT/include/cdi.inc" ; then
   CDI_INCLUDE='-I$(CDIROOT)/include'
else
   echo "cannot read $CDIROOT/include/cdi.inc"
   exit 1
fi

dnl ######################################################################
dnl Checks for UNITRANS.
dnl ######################################################################

UNITRANS_LIB=""
UNITRANS_INCLUDE=""

AC_ARG_WITH([unitrans],
            [AC_HELP_STRING([--with-unitrans=DIR],
                            [Enable experimental MPI transpose library support])],,
                            withval=no)

if test x$withval != xno; then
  if test x$with_unitrans != x; then
    UNITRANSROOT=$with_unitrans
  fi

  AC_CHECK_FILE($UNITRANSROOT/include/unitrans.mod,[],AC_MSG_ERROR([UNITRANSROOT not properly defined]))
  if test -n "$UNITRANSROOT" ; then
    if test -r "$UNITRANSROOT/lib/libunitrans.a" ; then
       UNITRANS_LIB='-L$(UNITRANSROOT)/lib -lunitrans'
    else
       echo "cannot read $UNITRANSROOT/lib/libunitrans.a"
       exit 1
    fi
  else
    echo "error: the variable UNITRANSROOT is not set in $host_frag"
    echo "       set UNITRANSROOT to the UNITRANS root directory"
    exit 1
  fi
fi
dnl ######################################################################
dnl Checks for POSTGRESQL.
dnl ######################################################################

POSTGRESQL_LIB=""
POSTGRESQL_INCLUDE=""

AC_ARG_WITH([postgresql],
            [AC_HELP_STRING([--with-postgresql=DIR],
                            [Enable collection of timings])],,
                            withval=no)

if test x$withval != xno; then
  if test x$with_postgresql != xyes; then
    POSTGRESQLROOT=$with_postgresql
  fi

  case $host in 
    *-*-darwin*)   
      pg_inc=$(find $POSTGRESQLROOT/include -name libpq-fe.h) 
      pg_inc=${pg_inc%\/libpq-fe.h}
      pg_inc=${pg_inc##$POSTGRESQLROOT/}
      pg_lib=$(find $POSTGRESQLROOT/lib -name libpq.a) 
      pg_lib=${pg_lib%\/libpq.a}
      pg_lib=${pg_lib##$POSTGRESQLROOT/}
      ;;
    x86_64-*-linux-*)
      pg_inc=$(find $POSTGRESQLROOT/include -name libpq-fe.h) 
      pg_inc=${pg_inc%\/libpq-fe.h}
      pg_inc=${pg_inc##$POSTGRESQLROOT/}
      pg_lib="lib"    
      ;;  
    *)
      pg_inc="include"    
      pg_lib="lib"    
      ;;    
  esac
  POSTGRESQL_INCLUDE="-I\$(POSTGRESQLROOT)/${pg_inc}"           

  AC_CHECK_HEADER($POSTGRESQLROOT/$pg_inc/libpq-fe.h,
                [AC_DEFINE([HAVE_POSTGRESQL], [1],
                   [Define to 1 if you have <libpq-fe.h>.])],
                AC_MSG_ERROR([POSTGRESQLROOT not properly defined/retrieved]))
  if test -n "$POSTGRESQLROOT" ; then
    if test -r "$POSTGRESQLROOT/$pg_lib/libpq.a" ; then
      case $fortran_compiler in
        gcc)
	  case $host_os in
            darwin*)
              POSTGRESQL_LIB="-L\$(POSTGRESQLROOT)/${pg_lib} -Wl,-rpath,\$(POSTGRESQLROOT)/${pg_lib} -lpq"
              ;;
            linux*)
              POSTGRESQL_LIB="-L\$(POSTGRESQLROOT)/${pg_lib} -Wl,-rpath=\$(POSTGRESQLROOT)/${pg_lib} -lpq"
	      ;;
	    esac  
          ;;
        pgi)
          POSTGRESQL_LIB="-L\$(POSTGRESQLROOT)/${pg_lib} -Wl,-rpath=\$(POSTGRESQLROOT)/${pg_lib} -lpq"
          ;;
        nag)  
          POSTGRESQL_LIB="-L\$(POSTGRESQLROOT)/${pg_lib} -Wl,-Wl,,-rpath -Wl,-Wl,\$(POSTGRESQLROOT)/${pg_lib} -lpq"
          ;;
        intel)
          POSTGRESQL_LIB="-L\$(POSTGRESQLROOT)/${pg_lib} -Wl,-rpath,\$(POSTGRESQLROOT)/${pg_lib} -lpq"
          ;;
        *)
          POSTGRESQL_LIB="-L\$(POSTGRESQLROOT)/${pg_lib} -lpq"
          ;;
      esac
    else
       echo "cannot read $POSTGRESQLROOT/$pg_lib/libpq.a"
       exit 1
    fi
  else
    echo "error: the variable POSTGRESQLROOT is not set in $host_frag"
    echo "       set POSTGRESQLROOT to the POSTGRESQL root directory"
    exit 1
  fi
else
  POSTGRESQLROOT=""
fi

dnl ######################################################################
dnl Checks for MPI.
dnl ######################################################################

if test x$use_mpi = xno ; then
  MPI="${DEFOPT}NOMPI"
  MPI_LIB=""
  MPI_VERSION=none
  MPIROOT=""
  use_mpi_compiler=no 
else
  case $host in
    powerpc-ibm-aix*)
      MPIROOT=/usr/lpp/ppe.poe    
      q64_result="no"
      mt_result="no"
      q64_result=$(echo $F90FLAGS | awk -v b="-q64" '$0 ~ b { print "yes" }')
      mt_result=$(echo $F90 | awk -v b="_r" '$0 ~ b { print "yes" } ')
      if [[ x$mt_result != xyes ]]; then
        echo "error: IBM MPI requires thread safe compiler versions."  
        echo "       add _r to compiler selected by F90."
        exit 1
      fi  

      if test x$q64_result = xyes; then
        MPI_INCLUDE_THREAD="thread64/"      
      else
        MPI_INCLUDE_THREAD="thread/"    
      fi
      ;;
    *-*-darwin*)   
      if test -n "$MPIROOT" ; then
        mpifh=$(find $MPIROOT/include -name mpif.h)
        mpifh=${mpifh%\/mpif.h}
        mpifh=${mpifh##*/}
        if test -n $mpifh; then
          MPI_INCLUDE_THREAD="$mpifh/"
        fi
      fi
      case $MPI_INCLUDE_THREAD in
      include*)
        MPI_INCLUDE_THREAD=""
        ;;
      esac  
      ;;
    *)
      MPI_INCLUDE_THREAD=""
      ;;
  esac

  MPI_INCLUDE=""

  if test x$with_mpi != x; then
    MPIROOT=$with_mpi
  fi    
  if test -n "$MPIROOT" ; then
    AC_CHECK_FILE($MPIROOT/include/${MPI_INCLUDE_THREAD}mpif.h,[],AC_MSG_ERROR([MPIROOT not properly defined]))
    MPI_LIB='-L$(MPIROOT)/lib '$MPI_LIB
    case $host in
    powerpc-ibm-aix*)
      use_mpi_compiler=yes
      mod_file=$(find $MPIROOT -name mpi.mod -print 2>/dev/null | grep mpi.mod | grep ${MPI_INCLUDE_THREAD}) 
      ;;
    sx?-nec-superux)
      real_path=$(cd $MPIROOT; pwd -P)
      mod_file=$(find $real_path -name mpi.mod -print | grep -v modulew)
      mod_file="${MPIROOT}/lib${mod_file##*lib0}"
      ;;
    *)
      real_path=$(cd $MPIROOT; pwd -P)
      mod_file=$(find $real_path -name mpi.mod -print -quit)
      ;;
    esac
    if test -r $mod_file ; then
       mod_file=${mod_file#$real_path}
       mod_file=${mod_file#$MPIROOT}       
       mod_file="\$(MPIROOT)"${mod_file}
       MPI_INCLUDE=${MODOPT}${mod_file%/*}   
    else  
       echo "cannot read $mod_file/mpi.mod"  
       exit 1    
    fi
  else
    MPI="${DEFOPT}NOMPI"
    MPI_LIB=""
  fi
fi

dnl ######################################################################
dnl  OASIS3 support:
dnl ######################################################################

PRISM_LIB=""
PRISM_INCLUDE=""
PRISM_CPP=""

AC_ARG_WITH([prism],
            [AC_HELP_STRING([--with-prism=DIR],
                            [Optional coupling package])],,
                            withval=no)

if test x$withval != xno ; then
   PRISMROOT=$with_prism 
   if test -n "$PRISMROOT" ; then
      PRISM="${DEFOPT}__coupled ${DEFOPT}__prism ${DEFOPT}use_comm_MPI1"  
   fi
   if test -r "$PRISMROOT/lib/libpsmile.MPI1.a" ; then  
      PRISM_LIB='-L$(PRISMROOT)/lib -lpsmile.MPI1 -lmpp_io'
   else
      echo "cannot read $PRISMROOT/lib/libpsmile.MPI1.a"
      exit 1  
   fi
   if test -d "$PRISMROOT/include" ; then
      PRISM_INCLUDE='-I$(PRISMROOT)/include'
   else
      echo "cannot find $PRISMROOT/include"
      exit 1
   fi
fi

dnl ######################################################################
dnl Checks for header files.

if test -z "$CROSS" ; then

   AC_HEADER_STDC   
   AC_CHECK_HEADERS_ONCE(fcntl.h fortran.h limits.h malloc.h netdb.h pwd.h unistd.h execinfo.h ucontext.h sys/param.h sys/time.h sys/unistd.h sys/utsname.h)

dnl Checks for typedefs, structures, and compiler characteristics.
   AC_C_CONST
   AC_HEADER_TIME

dnl Checks for library functions.
   AC_CHECK_FUNCS(getrusage gettimeofday sysconf uname valloc)

dnl Check for byte ordering

   AC_C_BIGENDIAN

dnl check for C datatypes

   AC_CHECK_SIZEOF(char)
   AC_CHECK_SIZEOF(short) 
   AC_CHECK_SIZEOF(int) 
   AC_CHECK_SIZEOF(long) 
   AC_CHECK_SIZEOF(long long) 
   AC_CHECK_SIZEOF(float) 
   AC_CHECK_SIZEOF(double) 
   AC_CHECK_SIZEOF(long double)
   AC_CHECK_SIZEOF([int *])

dnl Check for Fortran naming convention

   AC_FC_WRAPPERS

fi # ! CROSS


LIBS=$package_library

if test -z "$AR" ; then
  AR="ar"
fi  
if test -z "$AS" ; then
  AS="as"
fi  
if test -z "$ARFLAGS" ; then
  ARFLAGS="crv"
fi


AC_ARG_ENABLE([profile],
  [AS_HELP_STRING([--enable-profile],
    [add compile options and libraries for profiling (IBM pSeries and NEC-SX, IBM requires additionally --with-hpm=DIR)])],
    [],
    [withval=no])

if test x$withval != xno; then
  case $host in
  powerpc-ibm-aix*)
    AC_ARG_WITH([hpm],
      [AS_HELP_STRING([--with-hpm=DIR],
        [enable experimental support for profiling (IBM hpm])],
      [],
      [withval=no])

    if test x$withval != xno; then
      HPMROOT=$with_hpm
      AC_CHECK_FILE($HPMROOT/include/libhpm.h,[],AC_MSG_ERROR([--with-hpm=DIR not properly defined]))
      HPM_INCLUDE="-I$HPMROOT/include"
      if test x$q64_result = xyes; then
        HPM_LIB="-L$HPMROOT/lib64 -lhpm -lpmapi -lm" 
      else
        HPM_LIB="-L$HPMROOT/lib -lhpm -lpmapi -lm" 
      fi
    else
      AC_MSG_ERROR([--with-hpm=DIR needs to be added])
    fi
    CFLAGS="${CFLAGS} ${HPM_INCLUDE} -WF,-D__hpm__ -WF,-D_PROFILE" 
    FFLAGS="${FFLAGS} ${HPM_INCLUDE} -WF,-D__hpm__ -WF,-D_PROFILE" 
    F90FLAGS="${F90FLAGS} ${HPM_INCLUDE} -WF,-D__hpm__ -WF,-D_PROFILE" 
    LIBS="$LIBS $HPM_LIB"
    ;;      
  sx*-*-*)
    CFLAGS="${CFLAGS} -ftrace -D_PROFILE" 
    FFLAGS="${FFLAGS} -ftrace -D_PROFILE" 
    F90FLAGS="${F90FLAGS} -ftrace -D_PROFILE" 
    ;;
  esac
fi

case $host in
powerpc-ibm-aix*)
  if test "$use_mpi_compiler" = yes ; then 
    CC=mpcc_r
    F90=mpxlf95_r
    F77=mpxlf_r
    MPIROOT=""
    MPI_INCLUDE=""
    MPI_LIB=""
  fi
  ;;      
esac

CFLAGS="${CFLAGS} ${ac_cflags_debug_option} "   

if [[ "x$use_openmp" = xenabled ]] ; then
   F90FLAGS="${F90FLAGS} ${OMPFLAG}"
fi

FC=$F90

AC_SUBST(ARCH,$ARCH)

AC_SUBST(UNITRANSROOT,$UNITRANSROOT)
AC_SUBST(UNITRANS_LIB,$UNITRANS_LIB)
AC_SUBST(UNITRANS_INCLUDE,$UNITRANS_INCLUDE)

AC_SUBST(POSTGRESQLROOT,$POSTGRESQLROOT)
AC_SUBST(POSTGRESQL_LIB,$POSTGRESQL_LIB)
AC_SUBST(POSTGRESQL_INCLUDE,$POSTGRESQL_INCLUDE)

AC_SUBST(PRISMROOT,$PRISMROOT)
AC_SUBST(PRISM_LIB,$PRISM_LIB)
AC_SUBST(PRISM_INCLUDE,$PRISM_INCLUDE)
AC_SUBST(PRISM,$PRISM)

AC_SUBST(CDIROOT,$CDIROOT)
AC_SUBST(CDI_LIB,$CDI_LIB)
AC_SUBST(CDI_INCLUDE,$CDI_INCLUDE)

AC_SUBST(NETCDFROOT,$NETCDFROOT)
AC_SUBST(NETCDF_LIB,$NETCDF_LIB)
AC_SUBST(NETCDF_INCLUDE,$NETCDF_INCLUDE)

AC_SUBST(HDF5ROOT,$HDF5ROOT)
AC_SUBST(HDF5_LIB,$HDF5_LIB)
AC_SUBST(HDF5_INCLUDE,$HDF5_INCLUDE)

AC_SUBST(SZIPROOT,$SZIPROOT)
AC_SUBST(SZIP_LIB,$SZIP_LIB)
AC_SUBST(SZIP_INCLUDE,$SZIP_INCLUDE)

AC_SUBST(ZLIBROOT,$ZLIBROOT)
AC_SUBST(ZLIB_LIB,$ZLIB_LIB)
AC_SUBST(ZLIB_INCLUDE,$ZLIB_INCLUDE)

AC_SUBST(MPIROOT,$MPIROOT)
AC_SUBST(MPI_LIB,$MPI_LIB)
AC_SUBST(MPI_INCLUDE,$MPI_INCLUDE)
AC_SUBST(MPI,$MPI)

AC_SUBST(AR,$AR)
AC_SUBST(AS,$AS)

AC_SUBST(CFLAGS,$CFLAGS)
AC_SUBST(FFLAGS,$FFLAGS)
AC_SUBST(F90FLAGS,$F90FLAGS)
AC_SUBST(ARFLAGS,$ARFLAGS)
AC_SUBST(LIBS,$LIBS)
AC_SUBST(MODOPT,$MODOPT)
LDFLAGS=`echo $F90FLAGS | sed 's/-xf95-cpp-input//g'`
AC_SUBST(LDFLAGS,$LDFLAGS)
AC_SUBST(SRCDIRS,$SRCDIRS)

AC_OUTPUT(Makefile)

#-----------------------------------------------------------------------------
case $host in
  xt?-cray-*)
    f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==2')
    ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
    ;;
  *-*-solaris2*)
    f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1' | cut -f2- -d" ")
    ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
    ;;
  sx*-*-*)
    f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1;NR==5' | tr -d "\n")
    ccversion=$($(echo sxc++ -V) 2>&1 | sed -e 's/.c.*$//' | sed -e 's/C++\/SX Compiler //' | sed -e 's/C\/C++ Compiler //' | awk 'NR==1;NR==3' | tr -d "\n")
    ;;
  *-ibm-aix*)
    f90version=$($(echo $F90 -qversion) 2>&1 | tr -d "\n" | sed 's/Version: / \(/' | sed 's/$/\)/')
    ccversion=$($(echo $CC -qversion) 2>&1 | tr -d "\n" | sed 's/Version: / \(/' | sed 's/$/\)/') 
    ;;
  x86_64-*-linux-*)
    case $fortran_compiler in
    default|nag)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    gcc)
      f90version=$($(echo $F90 --version) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    intel)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    path)
      f90version=$($(echo $F90 -v) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    pgi)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==2')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    sun)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1' | cut -f2- -d" ")
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    esac
    ;;
  i*86-*-linux-*)      
    case $fortran_compiler in
    default|nag)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    gcc)
      f90version=$($(echo $F90 --version) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    intel)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    pgi)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==2')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    sun)
      f90version=$($(echo $F90 -V) 2>&1 | awk 'NR==1' | cut -f2- -d" ")
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
    esac
    ;;
  *-*-darwin*)
      f90version=$($(echo $F90 --version) 2>&1 | awk 'NR==1')
      ccversion=$($(echo $CC --version) 2>&1 | awk 'NR==1')
      ;;
  *)
    f90version="unknown"
    ccversion="unknown"
    ;;
esac
#-----------------------------------------------------------------------------
echo ""
echo "Selected compiler:"
echo ""
echo "    Fortran   : ${f90version}"
echo "    C         : ${ccversion}"
echo ""
echo "    OpenMP    : ${use_openmp}"
echo ""
case $host in
powerpc-ibm-aix*)     
    check_version=$( $F90 -qversion 2>&1 | awk '/^Version:/ {print $2}' )
    AS_VERSION_COMPARE([$check_version], [13.01.0000.0002], [dnl
        AS_BOX([ERROR: only IBM XLF 13.1.0.2 (EFIXed) or above is supported.])
        rm -f Makefile 
        AS_EXIT(1)])
    ;;  
esac
#-----------------------------------------------------------------------------
if test x$MPIROOT = x || test x$CROSS = xyes; then
case $host in
powerpc-ibm-aix*)
  if test "$use_mpi_compiler" = yes ; then
    mpi_version_number=$(lslpp -l ppe.poe | grep ppe.poe | uniq | awk '{print $2}')
    MPI_VERSION="IBM MPI ${mpi_version_number}"
  fi
  ;;
sx?-nec-superux)
  mpi_version_number=$(strings $MPIROOT/lib/libmpi.a | grep "MPI/SX: library Version" | awk '{print $4}')
  MPI_VERSION="SX MPI ${mpi_version_number}"
  ;;
*)
  MPI_VERSION=none
  ;;
esac
else
case $host in
*-*-darwin*)
  mpi_version_number=$(openmpiexec -V 2>&1 | awk 'NR==1' | cut -f3- -d" ")
  MPI_VERSION="openmpi ${mpi_version_number} (MacPorts)"
  ;;                   
*)
  MPI_VERSION=${MPIROOT##*/}
esac
fi  
#-----------------------------------------------------------------------------
echo "Selected libraries:"
echo ""
echo "    MPI       : ${MPI_VERSION}"
echo "                installation  ${MPIROOT}"
if test x$NETCDFROOT = x; then
echo "    netCDF    : none (error)"
else
netcdf_version=$(${NETCDFROOT}/bin/ncdump 2>&1 | grep "netcdf library version" | cut -f4 -d" ")
echo "    NETCDF    : netcdf-${netcdf_version}"
echo "                installation  ${NETCDFROOT}"
fi  
if test x$HDF5ROOT = x; then
echo "    HDF5      : none (error)"
else
hdf5_version=$(grep "define H5_VERSION" ${HDF5ROOT}/include/H5pubconf.h | cut -f2 -d"\"")
echo "    HDF5      : hdf5-${hdf5_version}"
echo "                installation  ${HDF5ROOT}"
fi  
if test x$SZIPROOT = x; then
echo "    SZIP      : none (error)"
else
szlib_version=$(grep "define SZLIB_VERSION" ${SZIPROOT}/include/szlib.h | cut -f2 -d"\"")
echo "    SZIP      : szip-${szlib_version}"
echo "                installation  ${SZIPROOT}"
fi  
if test x$ZLIBROOT = x; then
echo "    ZLIB      : none (error)"
else
zlib_version=$(grep "define ZLIB_VERSION" ${ZLIBROOT}/include/zlib.h | cut -f2 -d"\"")
echo "    ZLIB      : zlib-${zlib_version}"
echo "                installation  ${ZLIBROOT}"
fi  
echo ""
if test x$POSTGRESQLROOT = x; then
echo "    POSTGRESQL: none"
else
pq_version=$(grep "define PACKAGE_VERSION" ${POSTGRESQLROOT}/${pg_inc}/pg_config.h | cut -f2 -d"\"")
echo "    POSTGRESQL: postgresql-${pq_version}"
echo "                installation ${POSTGRESQLROOT}"
fi  
echo ""
if test x$PRISMROOT = x; then
echo "    PRISM     : none"
else
echo "    PRISM     : ${PRISMROOT}"
fi  
echo ""
if test x$UNITRANSROOT = x; then
echo "    UNITRANS  : none"
else
echo "    UNITRANS  : ${UNITRANSROOT}"
fi  
echo ""
echo ""
echo "Configuration done. Run make (GNU make) to build ECHAM."
echo ""

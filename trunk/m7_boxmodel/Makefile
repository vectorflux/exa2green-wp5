export

SHELL = /bin/sh
 
ARCH  = LINUX

srcdir = .
top_srcdir = .

prefix = .
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datarootdir = ${prefix}/share
datadir = ${datarootdir}
sysconfdir = ${prefix}/etc
libdir = ${exec_prefix}/lib
includedir = ${prefix}/include
oldincludedir = /usr/include
infodir = ${datarootdir}/info
mandir = ${datarootdir}/man

sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var

program_transform_name = s,x,x,

MPIROOT            = /opt/cray/mpt/5.6.0/gni/mpich2-intel/120
MPI_LIB            = -L$(MPIROOT)/lib 
MPI_INCLUDE        = -I$(MPIROOT)/include

POSTGRESQLROOT     = 
POSTGRESQL_LIB     = 
POSTGRESQL_INCLUDE = 

UNITRANSROOT       = 
UNITRANS_LIB       = 
UNITRANS_INCLUDE   = 

PRISMROOT          = 
PRISM_LIB          = 
PRISM_INCLUDE      = 

NETCDFROOT         = /opt/cray/netcdf/4.1.3/intel/120
NETCDF_LIB         = -L$(NETCDFROOT)/lib -lnetcdff -lnetcdf
NETCDF_INCLUDE     = -I$(NETCDFROOT)/include

HDF5ROOT           = /opt/cray/hdf5/1.8.8/intel/120
HDF5_LIB           = -L$(HDF5ROOT)/lib -lhdf5_hl -lhdf5
HDF5_INCLUDE       = -I$(HDF5ROOT)/include

SZIPROOT           = /apps/rosa/szip/2.1/int_1203
SZIP_LIB           = -L$(SZIPROOT)/lib -lsz
SZIP_INCLUDE       = -I$(SZIPROOT)/include

ZLIBROOT           = /usr
ZLIB_LIB           = -L$(ZLIBROOT)/lib64 -lz
ZLIB_INCLUDE       = -I$(ZLIBROOT)/include

LIB      = -L../lib -lsupport
LIBS     = $(LIB) $(NETCDF_LIB) $(HDF5_LIB) $(SZIP_LIB) $(ZLIB_LIB) $(PRISM_LIB) $(UNITRANS_LIB) $(MPI_LIB) $(POSTGRESQL_LIB)

MODOPT   = -I
MODULES  = ./

INCLUDE  = ../include
INCLUDES = $(MODOPT)$(MODULES) -I$(INCLUDE) $(NETCDF_INCLUDE) $(HDF5_INCLUDE) $(SZIP_INCLUDE) $(ZLIB_INCLUDE) $(PRISM_INCLUDE) $(UNITRANS_INCLUDE) $(MPI_INCLUDE)

F90      = ftn
FC       = ftn
CC       = cc
CPP      = 
AR       = ar
AS       = as 

DEFS     = -DHAVE_CONFIG_H

CFLAGS   = -I../config $(POSTGRESQL_INCLUDE) -std=gnu99 -O2 -DpgiFortran -D__XE6__  
FFLAGS   = -O2 -fpp -fp-model precise -fp-model source 
F90FLAGS = $(INCLUDES) -DLITTLE_ENDIAN -O2 -DHAVE_LIBNETCDF -fpp -fp-model precise -fp-model source  -DHAMMOZ  
CPPFLAGS = 
ARFLAGS  = crv
LDFLAGS  = -DLITTLE_ENDIAN -O2 -DHAVE_LIBNETCDF -fpp -fp-model precise -fp-model source -DHAMMOZ

SRCDIRS =  support src

all:
	@for DIR in $(SRCDIRS) ;\
	  do \
	    back=`pwd`; \
	    cd $$DIR ;\
	    $(MAKE) ; status=$$? ; \
	    if [ $$status != 0 ] ; then \
	      echo "Exit status from make was $$status" ; exit $$status ; \
	    fi ; \
	    cd $$back ; \
	  done 

clean:
	@for DIR in $(SRCDIRS) ;\
	  do \
	  (cd $$DIR ;\
	  $(MAKE) clean ; if [ $$? != 0 ] ; then \
	        echo "Exit status from make was $$?" ; exit 1 ; fi ;) ;\
	  done
	-rm -f config.cache
	-rm -f lib/*.a bin/m7_box
	-rm -f html/[a-z]*

distclean:
	@for DIR in $(SRCDIRS) ;\
	  do \
	  (cd $$DIR ;\
	  $(MAKE) clean ; if [ $$? != 0 ] ; then \
	        echo "Exit status from make was $$?" ; exit 1 ; fi ;) ;\
	  done
	-rm -f Makefile
	-rm -f config.log
	-rm -f config.cache
	-rm -f config.status
	-rm -f config/config.h
	-rm -f Makefile
	-rm -f lib/*.a bin/m7_box
	-rm -f html/[a-z]*

tar:
	@tarfile=../m7_box.f90.`date +%y%m%d`.taz ; gtar zcvf $$tarfile \
	`ls */*.f90 */*.[fhc] */*inc */Makefile Makefile.in Makefile run/hjob*`

index:
	-rm -f html/[a-z]*
	util/f2html.pl -f util/fgenrc -d html \
          support src include

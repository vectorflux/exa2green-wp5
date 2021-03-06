#------------------------------------------------------------------------------
# @file Makefile
# @author charlesj
# @date 2014-12-18 14:51:39.636080
# @brief Makefile for Fortran90 compilation with OpenMP acceleration
# 
# Makefile for Fortran90 compilation with OpenMP acceleration
# 
# This file was generated by Kppa: http://www.paratools.com/Kppa
#------------------------------------------------------------------------------


FC = gfortran
LD = $(FC)

OPT_FLAGS = -O3 -fopenmp
WARN_FLAGS = -Wall
PREC_FLAGS = -fdefault-real-8 -fdefault-double-8

FFLAGS = $(OPT_FLAGS) $(WARN_FLAGS) $(PREC_FLAGS)
LDFLAGS = $(OPT_FLAGS)
LIBS =

OBJ = kppa_OpenMP_integrate.o \
      kppa_OpenMP_jacobian.o \
      kppa_OpenMP_main.o \
      kppa_OpenMP_parameters.o \
      kppa_OpenMP_rosenbrock.o \
      kppa_OpenMP_blas.o \
      kppa_OpenMP_rates.o \
      kppa_OpenMP_function.o \
      kppa_OpenMP_sparse.o \
      kppa_OpenMP_decomp.o \
      kppa_OpenMP_solve.o \
      kppa_OpenMP_initialize.o \
      kppa_OpenMP_driver_parameters.o \
      kppa_OpenMP_monitor.o


EXE = kppa_OpenMP.exe

all: $(EXE)

$(EXE):  $(OBJ)
	$(LD) -o $(EXE) $(LDFLAGS) $(OBJ) $(LIBS)



kppa_OpenMP_integrate.o: kppa_OpenMP_integrate.f90 \
        kppa_OpenMP_parameters.mod \
        kppa_OpenMP_rosenbrock.o
	$(FC) -c $(FFLAGS) kppa_OpenMP_integrate.f90 

kppa_OpenMP_jacobian.o: kppa_OpenMP_jacobian.f90 \
        kppa_OpenMP_parameters.mod \
        kppa_OpenMP_sparse.o
	$(FC) -c $(FFLAGS) kppa_OpenMP_jacobian.f90 

kppa_OpenMP_main.o: kppa_OpenMP_main.f90 \
        kppa_OpenMP_parameters.mod \
        kppa_OpenMP_integrate.o \
        kppa_OpenMP_initialize.o \
        kppa_OpenMP_driver_parameters.mod \
        kppa_OpenMP_monitor.o
	$(FC) -c $(FFLAGS) kppa_OpenMP_main.f90 

kppa_OpenMP_parameters.mod: kppa_OpenMP_parameters.f90
	$(FC) -c $(FFLAGS) kppa_OpenMP_parameters.f90

kppa_OpenMP_rosenbrock.o: kppa_OpenMP_rosenbrock.f90 \
        kppa_OpenMP_parameters.mod \
        kppa_OpenMP_blas.o \
        kppa_OpenMP_rates.o \
        kppa_OpenMP_function.o \
        kppa_OpenMP_decomp.o \
        kppa_OpenMP_solve.o \
        kppa_OpenMP_jacobian.o
	$(FC) -c $(FFLAGS) kppa_OpenMP_rosenbrock.f90 

kppa_OpenMP_blas.o: kppa_OpenMP_blas.f90 \
        kppa_OpenMP_parameters.mod
	$(FC) -c $(FFLAGS) kppa_OpenMP_blas.f90 

kppa_OpenMP_rates.o: kppa_OpenMP_rates.f90 \
        kppa_OpenMP_parameters.mod
	$(FC) -c $(FFLAGS) kppa_OpenMP_rates.f90 

kppa_OpenMP_function.o: kppa_OpenMP_function.f90 \
        kppa_OpenMP_parameters.mod \
        kppa_OpenMP_sparse.o
	$(FC) -c $(FFLAGS) kppa_OpenMP_function.f90 

kppa_OpenMP_sparse.o: kppa_OpenMP_sparse.f90
	$(FC) -c $(FFLAGS) kppa_OpenMP_sparse.f90 

kppa_OpenMP_decomp.o: kppa_OpenMP_decomp.f90 \
        kppa_OpenMP_parameters.mod \
        kppa_OpenMP_sparse.o
	$(FC) -c $(FFLAGS) kppa_OpenMP_decomp.f90 

kppa_OpenMP_solve.o: kppa_OpenMP_solve.f90 \
        kppa_OpenMP_parameters.mod \
        kppa_OpenMP_sparse.o
	$(FC) -c $(FFLAGS) kppa_OpenMP_solve.f90 

kppa_OpenMP_initialize.o: kppa_OpenMP_initialize.f90 \
        kppa_OpenMP_parameters.mod
	$(FC) -c $(FFLAGS) kppa_OpenMP_initialize.f90 

kppa_OpenMP_driver_parameters.mod: kppa_OpenMP_driver_parameters.f90
	$(FC) -c $(FFLAGS) kppa_OpenMP_driver_parameters.f90

kppa_OpenMP_monitor.o: kppa_OpenMP_monitor.f90 \
        kppa_OpenMP_parameters.mod
	$(FC) -c $(FFLAGS) kppa_OpenMP_monitor.f90 


clean:
	rm -f kppa_OpenMP_integrate.o kppa_OpenMP_jacobian.o kppa_OpenMP_main.o kppa_OpenMP_parameters.o kppa_OpenMP_rosenbrock.o kppa_OpenMP_blas.o kppa_OpenMP_rates.o kppa_OpenMP_function.o kppa_OpenMP_sparse.o kppa_OpenMP_decomp.o kppa_OpenMP_solve.o kppa_OpenMP_initialize.o kppa_OpenMP_driver_parameters.o kppa_OpenMP_monitor.o
	rm -f kppa_OpenMP_integrate.mod kppa_OpenMP_jacobian.mod kppa_OpenMP_parameters.mod kppa_OpenMP_rosenbrock.mod kppa_OpenMP_blas.mod kppa_OpenMP_rates.mod kppa_OpenMP_function.mod kppa_OpenMP_sparse.mod kppa_OpenMP_decomp.mod kppa_OpenMP_solve.mod kppa_OpenMP_initialize.mod kppa_OpenMP_driver_parameters.mod kppa_OpenMP_monitor.mod
	rm -f kppa_OpenMP.exe
	
destroy: clean
	rm -f kppa_OpenMP_integrate.f90 kppa_OpenMP_jacobian.f90 kppa_OpenMP_main.f90 kppa_OpenMP_parameters.f90 kppa_OpenMP_rosenbrock.f90 kppa_OpenMP_blas.f90 kppa_OpenMP_rates.f90 kppa_OpenMP_function.f90 kppa_OpenMP_sparse.f90 kppa_OpenMP_decomp.f90 kppa_OpenMP_solve.f90 kppa_OpenMP_initialize.f90 kppa_OpenMP_driver_parameters.f90 kppa_OpenMP_monitor.f90 Makefile


#----------------------------- END Makefile END -------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: Set here the F90 compiler and options
#       Pedefined compilers: INTEL, PGF, HPUX, LAHEY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COMPILER = MPI
#COMPILER = MPI_VESUV
#COMPILER = GNU
#COMPILER = LAHEY
#COMPILER = INTEL
#COMPILER = PGF
#COMPILER = HPUX

FC_MPI_VESUV   = mpfort
FOPT_MPI_VESUV = -qsclk=micro -qfree=f90 -qsuffix=cpp=f90 -O3 -qhot -qmaxmem=-1 -w -qnosave -qsmp=omp -qthreaded -qsimd=auto -qaltivec -q64 -qarch=pwr8 -qtune=pwr8 -qlist -qreport 
#-qstrict=ieeefp -qieee -qfloat=nans -fopenmp -cpp -O3 -pg -fbounds-check -fimplicit-none  -Wall -fPIC
#FOPT_MPI_VESUV = -qsclk=micro -qfree=f90 -qsuffix=cpp=f90 -O3 -qhot -qstrict -qmaxmem=-1 -w -qnosave -qsmp=omp -qthreaded -qsimd=auto -qaltivec -q64 -qarch=pwr8 -qtune=pwr8 -qlist -qreport 
#FOPT_MPI_VESUV = -qsclk=micro -qfree=f90 -qsuffix=cpp=f90 -O3 -qhot=level=2 -qmaxmem=-1 -w -qnosave -qsmp=omp -qthreaded -qsimd=auto -qaltivec -q64 -qarch=pwr8 -qtune=pwr8 -qlist -qreport
#FOPT_MPI_VESUV = -pg -qsclk=micro -qfree=f90 -qsuffix=cpp=f90  -qmaxmem=-1 -w -qnosave -qsmp=omp -qthreaded -qsimd=auto -qaltivec -q64 -qarch=pwr8 -qtune=pwr8 -qlist -qreport
# "-WF,-DFINE_TIMING"
FC_MPI     = ftn
FOPT_MPI   = -fopenmp -cpp -O3 -pg -fbounds-check -fimplicit-none -ffree-line-length-none -Wall -fPIC
FC_GNU     = gfortran
FOPT_GNU   = -cpp -O -pg -fbounds-check -fimplicit-none  -Wall -fPIC
FC_LAHEY   = lf95
#FOPT_LAHEY = -Cpp --pca -O
FOPT_LAHEY = -Cpp --chk a,e,s,u --pca --ap -O0 -g --trap --trace --chkglobal
FC_INTEL   = ftn
#FOPT_INTEL = -fpp -O3 -openmp -fp-model strict (very slow)
FOPT_INTEL = -fpp -O1 -openmp
FC_PGF     = pgf90
#FOPT_PGF   = -Mpreprocess -O -fast -pc 80 -Kieee
FOPT_PGF   = -Mpreprocess -O0 -Mbounds -Mchkfpstk -Mchkptr -Mchkstk \
             -Ktrap=fp -pc 80 -Kieee
FC_HPUX    = f90
FOPT_HPUX  = -O -u +Oall +check=on

# define FULL_ALGEBRA for non-sparse integration
FC   = $(FC_$(COMPILER))
FOPT = $(FOPT_$(COMPILER)) # "-WF,-DFULL_ALGEBRA"

INCLUDES_MPI_VESUV = -I/fl/acm/Software/Libraries/vesuv/netcdf-4.3.2-xl-vesuv/include -I/dataL/acm/AMASS-git/exponential
LIBS_MPI_VESUV     = -L/fl/acm/Software/Libraries/vesuv/netcdf-4.3.2-xl-vesuv/lib/ -lnetcdff -lnetcdf -L/fl/acm/Software/Libraries/vesuv/hdf5-1.8.14-xl-vesuv/lib -lhdf5_fortran -lhdf5_hl -lhdf5 -lz /fl/acm/Software/Exa2Green-git/amester/amester/myscripts/src/libamester.a /dataL/acm/AMASS-git/exponential/libfastexp_scalar.a -lesslsmp -lmassvp8 -lmass_simdp8 -lmass
#LIBS = -llapack -lblas

INCLUDES_MPI = 
LIBS_MPI = -lnetcdff

INCLUDES_INTEL = 
LIBS_INTEL = -lnetcdff

INCLUDES_GNU = -I/usr/include/mpi 
LIBS_GNU     = -lnetcdff -lnetcdf -lhdf5_fortran -lhdf5_hl -lhdf5 -lz

INCLUDES   = $(INCLUDES_$(COMPILER))
LIBS       = $(LIBS_$(COMPILER))

# Command to create Matlab mex gateway routines 
# Note: use $(FC) as the mex Fortran compiler
MEX  = mex

GENSRC = kpp_Precision.f90  \
	 kpp_Parameters.f90     \
	 kpp_Global.f90  

GENOBJ = kpp_Precision.o    \
	 kpp_Parameters.o       \
	 kpp_Global.o     

FUNSRC = kpp_Function.f90 
FUNOBJ = kpp_Function.o 

JACSRC = kpp_JacobianSP.f90  kpp_Jacobian.f90
JACOBJ = kpp_JacobianSP.o    kpp_Jacobian.o

UTLSRC = kpp_Rates.f90 kpp_Util.f90 kpp_Monitor.f90
UTLOBJ = kpp_Rates.o   kpp_Util.o   kpp_Monitor.o

LASRC  = kpp_LinearAlgebra.f90 
LAOBJ  = kpp_LinearAlgebra.o   

STOCHSRC = kpp_Stochastic.f90 
STOCHOBJ = kpp_Stochastic.o 

MAINSRC = kpp_Main.f90   kpp_Initialize.f90   kpp_Integrator.f90 kpp_Model.f90
MAINOBJ = kpp_Main.o     kpp_Initialize.o     kpp_Integrator.o   kpp_Model.o 

WRAPNCSRC = Wrap_NETCDF.f90
WRAPNCOBJ = Wrap_NETCDF.o

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       objects needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALLOBJ = $(WRAPNCOBJ) $(GENOBJ) $(FUNOBJ) $(JACOBJ) \
	 $(UTLOBJ) $(LAOBJ) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       executables needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all:    exe

exe:	$(WRAPNCOBJ) $(ALLOBJ) $(MAINOBJ)
	$(FC) $(FOPT) $(ALLOBJ) $(MAINOBJ) $(LIBS) -o kpp.exe

stochastic:$(ALLOBJ) $(STOCHOBJ) $(MAINOBJ)
	$(FC) $(FOPT) $(ALLOBJ) $(STOCHOBJ) $(MAINOBJ) $(LIBS) \
	-o kpp_stochastic.exe

mex:    $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O kpp_mex_Fun.f90     $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O kpp_mex_Jac_SP.f90  $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O kpp_mex_Hessian.f90 $(ALLOBJ)

clean:
	rm -f kpp*.o kpp*.mod wrap_netcdf.mod Wrap_NETCDF.o \
	kpp*.dat kpp.exe kpp*.mexglx \
	kpp.map gmon.out Makefile_kpp \
	boxmodel.* *.txt *~

distclean:
	rm -f kpp*.o kpp*.mod wrap_netcdf.mod Wrap_NETCDF.* \
	kpp*.dat kpp.exe kpp.map \
	kpp*.f90 kpp_*.mexglx \
	gmon.out Makefile_kpp \
	boxmodel.* *.txt *~ Makefile

kpp_Precision.o: kpp_Precision.f90 
	$(FC) $(FOPT) -c $<

kpp_Parameters.o: kpp_Parameters.f90 \
	            kpp_Precision.o
	$(FC) $(FOPT) -c $<

kpp_Monitor.o: kpp_Monitor.f90 \
	            kpp_Precision.o
	$(FC) $(FOPT) -c $<

kpp_Global.o: kpp_Global.f90 \
	            kpp_Parameters.o kpp_Precision.o
	$(FC) $(FOPT) -c $<

kpp_Initialize.o: kpp_Initialize.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

kpp_Function.o: kpp_Function.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

kpp_Stochastic.o: kpp_Stochastic.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

kpp_JacobianSP.o: kpp_JacobianSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

kpp_Jacobian.o: kpp_Jacobian.f90  $(GENOBJ) kpp_JacobianSP.o
	$(FC) $(FOPT) -c $<

kpp_LinearAlgebra.o: kpp_LinearAlgebra.f90 $(GENOBJ) kpp_JacobianSP.o
	$(FC) $(FOPT) -c $<

kpp_Rates.o: kpp_Rates.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $(INCLUDES) $<

kpp_HessianSP.o: kpp_HessianSP.f90  $(GENOBJ)
	$(FC) $(FOPT) -c $<

kpp_Hessian.o:  kpp_Hessian.f90 $(GENOBJ) kpp_HessianSP.o
	$(FC) $(FOPT) -c $<

kpp_StoichiomSP.o: kpp_StoichiomSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

kpp_Stoichiom.o: kpp_Stoichiom.f90  $(GENOBJ) kpp_StoichiomSP.o
	$(FC) $(FOPT) -c $<

kpp_Util.o: kpp_Util.f90  $(GENOBJ) kpp_Monitor.o
	$(FC) $(FOPT) -c $<

Wrap_NETCDF.o: Wrap_NETCDF.f90 $(GENOBJ)
	$(FC) $(FOPT) -c -I/usr/include/ $(INCLUDES) $<

kpp_Main.o: kpp_Main.f90  $(ALLOBJ) kpp_Initialize.o kpp_Model.o kpp_Integrator.o Wrap_NETCDF.o
	$(FC) $(FOPT) -c -I/usr/include/ $(INCLUDES) $<

kpp_Model.o: kpp_Model.f90  $(ALLOBJ) kpp_Integrator.o
	$(FC) $(FOPT) -c $<

kpp_Integrator.o: kpp_Integrator.f90  $(ALLOBJ)
	$(FC) $(FOPT) -c $<

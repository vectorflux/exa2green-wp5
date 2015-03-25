#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: Set here the F90 compiler and options
#       Pedefined compilers: INTEL, PGF, HPUX, LAHEY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COMPILER = MPI
#COMPILER = GNU
#COMPILER = LAHEY
#COMPILER = INTEL
#COMPILER = PGF
#COMPILER = HPUX

FC_MPI = mpif90
FOPT_MPI = -cpp -O3 -pg -fbounds-check -fimplicit-none  -Wall -fPIC
FC_GNU     = gfortran
FOPT_GNU   = -cpp -O -pg -fbounds-check -fimplicit-none  -Wall -fPIC
FC_LAHEY   = lf95
#FOPT_LAHEY = -Cpp --pca -O
FOPT_LAHEY = -Cpp --chk a,e,s,u --pca --ap -O0 -g --trap --trace --chkglobal
FC_INTEL   = ifort 
#FOPT_INTEL = -cpp -O -mp -pc80 -prec_div -tpp7
FOPT_INTEL = -cpp -O0 -mp -implicitnone -ftrapuv \
              -debug all -check all -warn all
FC_PGF     = pgf90
#FOPT_PGF   = -Mpreprocess -O -fast -pc 80 -Kieee
FOPT_PGF   = -Mpreprocess -O0 -Mbounds -Mchkfpstk -Mchkptr -Mchkstk \
             -Ktrap=fp -pc 80 -Kieee
FC_HPUX    = f90
FOPT_HPUX  = -O -u +Oall +check=on

# define FULL_ALGEBRA for non-sparse integration
FC   = $(FC_$(COMPILER))
FOPT = $(FOPT_$(COMPILER)) # -DFULL_ALGEBRA

LIBS =
#LIBS = -llapack -lblas

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

HESSRC = kpp_HessianSP.f90   kpp_Hessian.f90
HESOBJ = kpp_HessianSP.o     kpp_Hessian.o


UTLSRC = kpp_Rates.f90 kpp_Util.f90 kpp_Monitor.f90
UTLOBJ = kpp_Rates.o   kpp_Util.o   kpp_Monitor.o

LASRC  = kpp_LinearAlgebra.f90 
LAOBJ  = kpp_LinearAlgebra.o   

STOCHSRC = kpp_Stochastic.f90 
STOCHOBJ = kpp_Stochastic.o 

MAINSRC = kpp_Main.f90   kpp_Initialize.f90   kpp_Integrator.f90 kpp_Model.f90
MAINOBJ = kpp_Main.o     kpp_Initialize.o     kpp_Integrator.o   kpp_Model.o 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       objects needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALLOBJ = $(GENOBJ) $(FUNOBJ) $(JACOBJ) \
	 $(UTLOBJ) $(LAOBJ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       executables needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all:    exe

exe:	$(ALLOBJ) $(MAINOBJ) 
	$(FC) $(FOPT) $(ALLOBJ) $(MAINOBJ) $(LIBS) -o kpp.exe

stochastic:$(ALLOBJ) $(STOCHOBJ) $(MAINOBJ)
	$(FC) $(FOPT) $(ALLOBJ) $(STOCHOBJ) $(MAINOBJ) $(LIBS) \
	-o kpp_stochastic.exe

mex:    $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O kpp_mex_Fun.f90     $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O kpp_mex_Jac_SP.f90  $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O kpp_mex_Hessian.f90 $(ALLOBJ)

clean:
	rm -f kpp*.o kpp*.mod \
	kpp*.dat kpp.exe kpp*.mexglx \
	kpp.map gmon.out Makefile_kpp

distclean:
	rm -f kpp*.o kpp*.mod \
	kpp*.dat kpp.exe kpp.map \
	kpp*.f90 kpp_*.mexglx \
	gmon.out Makefile_kpp

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
	$(FC) $(FOPT) -c $<

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

kpp_Main.o: kpp_Main.f90  $(ALLOBJ) kpp_Initialize.o kpp_Model.o kpp_Integrator.o
	$(FC) $(FOPT) -c $<

kpp_Model.o: kpp_Model.f90  $(ALLOBJ) kpp_Integrator.o
	$(FC) $(FOPT) -c $<

kpp_Integrator.o: kpp_Integrator.f90  $(ALLOBJ)
	$(FC) $(FOPT) -c $<

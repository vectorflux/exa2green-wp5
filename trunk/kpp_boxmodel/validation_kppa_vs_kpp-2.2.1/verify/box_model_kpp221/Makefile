#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: Set here the F90 compiler and options
#       Pedefined compilers: INTEL, PGF, HPUX, LAHEY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

COMPILER = GNU
#COMPILER = LAHEY
#COMPILER = INTEL
#COMPILER = PGF
#COMPILER = HPUX

PREC_FLAGS = -fdefault-real-8 -fdefault-double-8

FC_GNU     = gfortran
FOPT_GNU   = -cpp -O -fbounds-check -fimplicit-none  -Wall -Wtabs $(PREC_FLAGS)
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

GENSRC = box_model_Precision.f90  \
	 box_model_Parameters.f90     \
	 box_model_Global.f90  

GENOBJ = box_model_Precision.o    \
	 box_model_Parameters.o       \
	 box_model_Global.o     

FUNSRC = box_model_Function.f90 
FUNOBJ = box_model_Function.o 

JACSRC = box_model_JacobianSP.f90  box_model_Jacobian.f90
JACOBJ = box_model_JacobianSP.o    box_model_Jacobian.o

HESSRC = box_model_HessianSP.f90   box_model_Hessian.f90
HESOBJ = box_model_HessianSP.o     box_model_Hessian.o

STMSRC = box_model_StoichiomSP.f90 box_model_Stoichiom.f90 
STMOBJ = box_model_StoichiomSP.o   box_model_Stoichiom.o

UTLSRC = box_model_Rates.f90 box_model_Util.f90 box_model_Monitor.f90
UTLOBJ = box_model_Rates.o   box_model_Util.o   box_model_Monitor.o

LASRC  = box_model_LinearAlgebra.f90 
LAOBJ  = box_model_LinearAlgebra.o   

STOCHSRC = box_model_Stochastic.f90 
STOCHOBJ = box_model_Stochastic.o 

MAINSRC = box_model_Main.f90   box_model_Initialize.f90   box_model_Integrator.f90 box_model_Model.f90
MAINOBJ = box_model_Main.o     box_model_Initialize.o     box_model_Integrator.o   box_model_Model.o 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       objects needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ALLOBJ = $(GENOBJ) $(FUNOBJ) $(JACOBJ) $(HESOBJ) $(STMOBJ) \
	 $(UTLOBJ) $(LAOBJ)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User: modify the line below to include only the
#       executables needed by your application
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all:    exe

exe:	$(ALLOBJ) $(MAINOBJ) 
	$(FC) $(FOPT) $(ALLOBJ) $(MAINOBJ) $(LIBS) -o box_model.exe

stochastic:$(ALLOBJ) $(STOCHOBJ) $(MAINOBJ)
	$(FC) $(FOPT) $(ALLOBJ) $(STOCHOBJ) $(MAINOBJ) $(LIBS) \
	-o box_model_stochastic.exe

mex:    $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O box_model_mex_Fun.f90     $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O box_model_mex_Jac_SP.f90  $(ALLOBJ)
	$(MEX) FC#$(FC) -fortran -O box_model_mex_Hessian.f90 $(ALLOBJ)

clean:
	rm -f box_model*.o box_model*.mod \
	box_model*.dat box_model.exe box_model*.mexglx \
	box_model.map

distclean:
	rm -f box_model*.o box_model*.mod \
	box_model*.dat box_model.exe box_model.map \
	box_model*.f90 box_model_*.mexglx

box_model_Precision.o: box_model_Precision.f90 
	$(FC) $(FOPT) -c $<

box_model_Parameters.o: box_model_Parameters.f90 \
	            box_model_Precision.o
	$(FC) $(FOPT) -c $<

box_model_Monitor.o: box_model_Monitor.f90 \
	             box_model_Precision.o
	$(FC) $(FOPT) -c $<

box_model_Global.o: box_model_Global.f90 \
	            box_model_Parameters.o box_model_Precision.o
	$(FC) $(FOPT) -c $<

box_model_Initialize.o: box_model_Initialize.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

box_model_Function.o: box_model_Function.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

box_model_Stochastic.o: box_model_Stochastic.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

box_model_JacobianSP.o: box_model_JacobianSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

box_model_Jacobian.o: box_model_Jacobian.f90  $(GENOBJ) box_model_JacobianSP.o
	$(FC) $(FOPT) -c $<

box_model_LinearAlgebra.o: box_model_LinearAlgebra.f90 $(GENOBJ) box_model_JacobianSP.o
	$(FC) $(FOPT) -c $<

box_model_Rates.o: box_model_Rates.f90  $(GENOBJ) 
	$(FC) $(FOPT) -c $<

box_model_HessianSP.o: box_model_HessianSP.f90  $(GENOBJ)
	$(FC) $(FOPT) -c $<

box_model_Hessian.o:  box_model_Hessian.f90 $(GENOBJ) box_model_HessianSP.o
	$(FC) $(FOPT) -c $<

box_model_StoichiomSP.o: box_model_StoichiomSP.f90 $(GENOBJ)
	$(FC) $(FOPT) -c $<

box_model_Stoichiom.o: box_model_Stoichiom.f90  $(GENOBJ) box_model_StoichiomSP.o
	$(FC) $(FOPT) -c $<

box_model_Util.o: box_model_Util.f90  $(GENOBJ) box_model_Monitor.o
	$(FC) $(FOPT) -c $<

box_model_Main.o: box_model_Main.f90  $(ALLOBJ) box_model_Initialize.o box_model_Model.o box_model_Integrator.o
	$(FC) $(FOPT) -c $<

box_model_Model.o: box_model_Model.f90  $(ALLOBJ) box_model_Integrator.o
	$(FC) $(FOPT) -c $<

box_model_Integrator.o: box_model_Integrator.f90  $(ALLOBJ)
	$(FC) $(FOPT) -c $<

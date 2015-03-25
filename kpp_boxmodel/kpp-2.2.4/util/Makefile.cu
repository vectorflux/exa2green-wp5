# Set here the desired CUDA compiler and its optimization options
NVCC   = nvcc
COPT = -O -Wall  

# Set here the Fortran77 compiler and the desired optimization options
COMPILER = GNU

FC_INTEL   = ifort
FOPT_INTEL = -O -f77rtl -mp -pc80 -prec_div -tpp7
FC_PGF	 = pgf77
FOPT_PGF   = -O -fast -pc 80 -Kieee
FC_GNU	 = g77
FOPT_GNU   = -O -Wall -Wimplicit -ffast-math -funroll-loops \
			 -malign-double -ffortran-bounds-check
FC_HPUX	= f90
FOPT_HPUX  = -O

FC   = $(FC_$(COMPILER))
FOPT = $(FOPT_$(COMPILER)) 

# To create Matlab gateway routines
# Note: use $(NVCC) as the mex C compiler
MEX  = mex

HEADERS = KPP_ROOT_Global.h  KPP_ROOT_Parameters.h  KPP_ROOT_Sparse.h  KPP_ROOT_CUDA_Utils.h

SPSRC = KPP_ROOT_JacobianSP.c \
	KPP_ROOT_HessianSP.c  \
	KPP_ROOT_StoichiomSP.c

SPOBJ = KPP_ROOT_JacobianSP.o \
	KPP_ROOT_HessianSP.o  \
	KPP_ROOT_StoichiomSP.o


SRC =   KPP_ROOT_Main.c	  KPP_ROOT_Integrator.c   \
	KPP_ROOT_Function.cu  KPP_ROOT_Initialize.c   \
	KPP_ROOT_Jacobian.c  KPP_ROOT_LinearAlgebra.c\
	KPP_ROOT_Rates.c	 KPP_ROOT_Hessian.c	  \
	KPP_ROOT_Stoichiom.c KPP_ROOT_Util.c		 \
	KPP_ROOT_Monitor.c

FSRC = KPP_ROOT_Integrator.f90

OBJ =   KPP_ROOT_Main.o	  KPP_ROOT_Integrator.o   \
	KPP_ROOT_Function.o  KPP_ROOT_Initialize.o   \
	KPP_ROOT_Jacobian.o  KPP_ROOT_LinearAlgebra.o\
	KPP_ROOT_Rates.o	 KPP_ROOT_Hessian.o	  \
	KPP_ROOT_Stoichiom.o KPP_ROOT_Util.o		 \
	KPP_ROOT_Monitor.o

FOBJ = KPP_ROOT_Integrator_f90.o

STOCHSRC = KPP_ROOT_Stochastic.c 
STOCHOBJ = KPP_ROOT_Stochastic.o 

all:	exe

exe:	$(HEADERS) $(SPOBJ) $(OBJ)
	$(NVCC) $(COPT) $(SPOBJ) $(OBJ) -lm -o KPP_ROOT.exe	

stochastic:$(HEADERS) $(SPOBJ) $(OBJ) $(STOCHOBJ)
	$(NVCC) $(COPT) $(SPOBJ) $(OBJ) $(STOCHOBJ) -lm \
	-o KPP_ROOT_stochastic.exe	

mex:	$(HEADERS) $(SPOBJ) $(OBJ)
	$(MEX) NVCC#$(NVCC) -O KPP_ROOT_mex_Fun.c	 -lm $(SPOBJ) $(OBJ)
	$(MEX) NVCC#$(NVCC) -O KPP_ROOT_mex_Jac_SP.c  -lm $(SPOBJ) $(OBJ)
	$(MEX) NVCC#$(NVCC) -O KPP_ROOT_mex_Hessian.c -lm $(SPOBJ) $(OBJ)


clean:
	rm -f $(SPOBJ) $(OBJ) $(FOBJ) KPP_ROOT.exe KPP_ROOT_*.mexglx KPP_ROOT*.dat

distclean:
	rm -f $(SPOBJ) $(OBJ) $(FOBJ) KPP_ROOT.exe KPP_ROOT*.dat \
	KPP_ROOT_*.c KPP_ROOT_*.cu KPP_ROOT_*.h KPP_ROOT_*.f90 KPP_ROOT_*.map KPP_ROOT_*.mexglx

KPP_ROOT_Monitor.o: KPP_ROOT_Monitor.c $(HEADERS)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_JacobianSP.o: KPP_ROOT_JacobianSP.c $(HEADERS)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_HessianSP.o: KPP_ROOT_HessianSP.c  $(HEADERS)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_StoichiomSP.o: KPP_ROOT_StoichiomSP.c $(HEADERS)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Main.o: KPP_ROOT_Main.c KPP_ROOT_Initialize.o $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Integrator.o: KPP_ROOT_Integrator.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Integrator_f90.o: KPP_ROOT_Integrator.f90 $(FC) $(FOPT) -c $<

KPP_ROOT_Initialize.o: KPP_ROOT_Initialize.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Function.o: KPP_ROOT_Function.cu  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Stochastic.o: KPP_ROOT_Stochastic.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Jacobian.o: KPP_ROOT_Jacobian.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_LinearAlgebra.o: KPP_ROOT_LinearAlgebra.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Rates.o: KPP_ROOT_Rates.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Hessian.o:  KPP_ROOT_Hessian.c $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Stoichiom.o: KPP_ROOT_Stoichiom.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<

KPP_ROOT_Util.o: KPP_ROOT_Util.c  $(HEADERS) $(SPOBJ)
	$(NVCC) $(COPT) -c $<


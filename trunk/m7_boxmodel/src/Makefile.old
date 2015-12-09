# Generated automatically by createMakefiles.pl
 
PROG =	../bin/m7_box

SRCS =	m7_box.f90 mo_constants.f90 mo_filename.f90 mo_ham.f90 \
	mo_ham_init.f90 mo_ham_kappa.f90 mo_ham_m7.f90 mo_ham_m7_species.f90 \
	mo_ham_m7ctl.f90 mo_ham_nucl.f90 mo_ham_soa.f90 mo_ham_species.f90 \
	mo_kind.f90 mo_namelist.f90 mo_netcdf.f90 mo_read_netcdf77.f90 \
	mo_species.f90 mo_time_control.f90 mo_tracdef.f90 mo_util_string.f90

OBJS := $(SRCS:.f90=.o)

all: $(PROG)

$(PROG): $(OBJS) ../lib/libsupport.a
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

m7_box.o: mo_ham.o mo_ham_init.o mo_ham_m7.o mo_ham_m7ctl.o mo_kind.o \
	mo_namelist.o $(INCLUDE)/ini_m7ctl.inc
mo_constants.o: mo_kind.o
mo_ham.o: mo_namelist.o mo_species.o mo_util_string.o $(INCLUDE)/hamctl.inc
mo_ham_init.o: mo_ham.o mo_ham_kappa.o mo_ham_m7_species.o mo_ham_m7ctl.o \
	mo_ham_nucl.o mo_ham_soa.o mo_ham_species.o mo_species.o
mo_ham_kappa.o: mo_constants.o mo_kind.o mo_netcdf.o mo_read_netcdf77.o
mo_ham_m7.o: mo_constants.o mo_ham.o mo_ham_kappa.o mo_ham_m7_species.o \
	mo_ham_m7ctl.o mo_ham_nucl.o mo_ham_soa.o mo_ham_species.o mo_kind.o \
	mo_species.o mo_time_control.o
mo_ham_m7_species.o: mo_ham.o mo_ham_m7ctl.o mo_ham_soa.o mo_ham_species.o \
	mo_species.o
mo_ham_m7ctl.o: mo_constants.o mo_ham.o mo_kind.o mo_namelist.o mo_species.o \
	$(INCLUDE)/ham_m7ctl.inc
mo_ham_nucl.o: mo_ham_m7ctl.o mo_kind.o mo_netcdf.o
mo_ham_soa.o: mo_ham.o mo_ham_species.o mo_kind.o mo_species.o mo_tracdef.o
mo_ham_species.o: mo_kind.o mo_species.o mo_tracdef.o
mo_namelist.o: mo_filename.o mo_util_string.o
mo_netcdf.o: mo_kind.o
mo_read_netcdf77.o: mo_kind.o mo_netcdf.o
mo_species.o: mo_kind.o mo_tracdef.o
mo_time_control.o: mo_kind.o
mo_tracdef.o: mo_kind.o
mo_util_string.o: mo_kind.o

ifeq ($(strip $(ARCH)), SX)
m7_dconc.o: m7_dconc.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_cumnor  expin=m7_cumnor.f90 -c m7_dconc.f90
mo_aero_tools.o: mo_aero_tools.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_cumnor  expin=m7_cumnor.f90 -c mo_aero_tools.f90
m7_delcoa.o: m7_delcoa.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_coat  expin=mo_aero_m7.f90 -c m7_delcoa.f90
m7_concoag.o: m7_concoag.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_coat  expin=mo_aero_m7.f90 -c m7_concoag.f90
mo_spitfire.o: mo_spitfire.f90
	$(F90) $(F90FLAGS) -pi auto exp=minmod exp=medan exp=putyslice exp=cfdot1dp2 exp=cfint1x2 line=2000 -c mo_spitfire.f90
mo_tpcore.o: mo_tpcore.f90
#warning: Don't change line to 2000. This will give wrong code on the SX!!!
	$(F90) $(F90FLAGS) -pi auto exp=xmist,fxppm,kmppm,lmppm,xtp noexp=map1_ppm_gp,ppm2m,steepz nest=3 line=1000 -c mo_tpcore.f90
mo_transpose.o: mo_transpose.f90
	$(F90) $(F90FLAGS) -Npi -c mo_transpose.f90
lti.o: lti.f90
	$(F90) $(F90FLAGS) -Npi -c lti.f90
endif


ifeq ($(strip $(ARCH)), ES)
m7_dconc.o: m7_dconc.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_cumnor  expin=m7_cumnor.f90 -c m7_dconc.f90
mo_aero_tools.o: mo_aero_tools.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_cumnor  expin=m7_cumnor.f90 -c mo_aero_tools.f90
m7_delcoa.o: m7_delcoa.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_coat  expin=mo_aero_m7.f90 -c m7_delcoa.f90
m7_concoag.o: m7_concoag.f90
	$(F90) $(F90FLAGS)  -pi auto line=1000 rexp=m7_coat  expin=mo_aero_m7.f90 -c m7_concoag.f90
mo_spitfire.o: mo_spitfire.f90
	$(F90) $(F90FLAGS) -pi auto exp=minmod exp=medan exp=putyslice exp=cfdot1dp2 exp=cfint1x2 line=2000 -c mo_spitfire.f90
mo_tpcore.o: mo_tpcore.f90
#warning: Don't change line to 2000. This will give wrong code on the SX!!!
	$(F90) $(F90FLAGS) -pi auto exp=xmist,fxppm,kmppm,lmppm,xtp noexp=map1_ppm_gp,ppm2m,steepz nest=3 line=1000 -c mo_tpcore.f90
mo_transpose.o: mo_transpose.f90
	$(F90) $(F90FLAGS) -Npi -c mo_transpose.f90
lti.o: lti.f90
	$(F90) $(F90FLAGS) -Npi -c lti.f90
endif

ifeq ($(strip $(ARCH)), CRAY_PVP)
mo_buffer_fft.o: mo_buffer_fft.f90
	$(F90) $(F90FLAGS) -Ovector1 -c mo_buffer_fft.f90
mo_grib.o: mo_grib.f90
	$(F90) $(F90FLAGS) -Ovector1 -c mo_grib.f90
endif

ifeq ($(strip $(ARCH)), CRAY_X1)
	$(F90) $(F90FLAGS) -Ovector1 -Oscalar1 -Ostream1 -c mo_io.f90
endif

ifeq ($(strip $(ARCH)), CRAY_XT3)
mo_transpose.o: mo_transpose.f90
	( F90FLAGS=" " ; $(F90) -O2  -c mo_transpose.f90 )
endif

ifeq ($(strip $(ARCH)), CRAY_XD1)
mo_transpose.o: mo_transpose.f90
	( F90FLAGS=" " ; $(F90) -O2  -c mo_transpose.f90 )
endif

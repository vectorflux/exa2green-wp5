#==============================================================================
#
# Database for target specific parameters
# ---------------------------------------
#
# The database contains parameters which are used for the configuration
# of the Makefile and the run script. These parameters are defined for 
# "targets", which are descriptive names referring typically to the
# computer system, the compiler or the model parallelization to be used.
#
# Marco Giorgetta, MPI-M, 2010-04-16
#
#==============================================================================
#
# Parameters
# ---------- 
#
# Configure and make:
#   site            = name of site, where configure and make is done
#   loadmodule      = name of Fortran compiler module to be loaded
#   configureoption = option(s) for configure command
#   mpiroot         = root directory of MPI
#   make_command    = "make" command with options, to be used for compilation
#
# Optimization and parallelization:
#   nproma          = inner loop length
#   nthreads        = number of threads for OpenMP parallelization
#   nprocs          = number of processes for MPI parallelization
#   nproca          = number of blocks in latitude dimension
#   nprocb          = number of blocks in longitude dimension
#   nnodes          = number of nodes to be used
#
# Run script:
#   header          = suffix of header section to be used for run script
#   builddir        = build directory relative to the ICON main directory
#   start           = command to start the model executable
#   submit          = command to submit the run script to a queueing system
#
#------------------------------------------------------------------------------

db_status=0 # exit status of database, 0=ok, 1=target not known

case ${target} in

    # Blizzard
    # --------
    blizzard | blizz_nMnO)
        site="dkrz"
        loadmodule="IBM/xlf13.1.0.2"
        configureoption="--with-mpi=no  --with-openmp=no"
        mpiroot=""
        make_command="make -j 4"
        nproma=72
        nthreads=1  # nO
        nprocs=1    # nM
        nproca=1    # "
        nprocb=1    # "
        nnodes=1
        header="blizz_serial"
        builddir="bin/echam6"
        start=""
        submit="llsubmit"
        ;;
    blizz_nMyO)
        site="dkrz"
        loadmodule="IBM/xlf13.1.0.2"
        configureoption="--with-mpi=no  --with-openmp=yes"
        mpiroot=""
        make_command="make -j 4"
        nproma=72
        nthreads=32 # yO
        nprocs=1    # nM
        nproca=1    # "
        nprocb=1    # "
        nnodes=1
        header="blizz_ompsmt"
        builddir="bin/echam6"
        start=""
        submit="llsubmit"
        ;;
    blizz_yMnO)
        site="dkrz"
        loadmodule="IBM/xlf13.1.0.2"
        configureoption="--with-openmp=no"
        mpiroot=""
        make_command="make -j 4"
        nproma=72
        nthreads=1  # nO
        nprocs=64   # yM
        nproca=4    # "
        nprocb=16   # "
        nnodes=1
        header="blizz_mpismt"
        builddir="bin/echam6"
        start="poe"
        submit="llsubmit"
        ;;
    blizz_yMyO)
        site="dkrz"
        loadmodule="IBM/xlf13.1.0.2"
        configureoption="-with-openmp=yes"
        mpiroot=""
        make_command="make -j 4"
        nproma=72
        nthreads=4  # yO
        nprocs=16   # yM
        nproca=2    # "
        nprocb=8    # "
        nnodes=1
        header="blizz_mpiompsmt"
        builddir="bin/echam6"
        start="poe"
        submit="llsubmit"
        ;;
    blizz_amip)
        site="dkrz"
        loadmodule="IBM/xlf13.1.0.2"
        configureoption="-with-openmp=yes"
        mpiroot=""
        make_command="make -j 32"
        nproma=72
        nthreads=2  # yO
        nprocs=32   # yM
        nproca=16   # "
        nprocb=8    # "
        nnodes=4
	account=mh0469
        header="blizz_mpiompsmt_amip"
        builddir="bin/echam6"
        start="poe"
        submit="llsubmit"
        ;;

    # MPIPC
    # -----
    mpipc | mpipc_gcc)
        site="mpi-m"
        loadmodule="GCC/4.3.3"
        configureoption="--with-fortran=gcc"
        mpiroot="/sw/etch-ia32/mpich2-1.2.1-gcc43"
        make_command="make -j 2"
        nproma=32
        nthreads=1
        nprocs=2
        nproca=1
        nprocb=2
        nnodes=1
        header="mpipc"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit=""
       ;;
    mpipc_nag)
        site="mpi-m"
        loadmodule="GCC NAG/5.2.721"
        configureoption="--with-fortran=nag"
        mpiroot="/sw/etch-ia32/mpich2-1.2.1-nag52"
        make_command="make -j 2"
        nproma=32
        nthreads=1
        nprocs=2
        nproca=1
        nprocb=2
        nnodes=1
        header="mpipc"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit=""
        ;;
    mpipc_intel)
        site="mpi-m"
        loadmodule="GCC Intel/11.1.064"
        configureoption="--with-fortran=intel"
        mpiroot="/sw/etch-ia32/mpich2-1.2.1-intel11"
        make_command="make -j 2"
        nproma=32
        nthreads=1
        nprocs=2
        nproca=1
        nprocb=2
        nnodes=1
        header="mpipc"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit=""
        ;;
    mpipc_pgi)
        site="mpi-m"
        loadmodule="GCC PGI/10.9"
        configureoption="--with-fortran=pgi"
        mpiroot="/sw/etch-ia32/mpich2-1.2.1-pgi9"
        make_command="make -j 2"
        nproma=32
        nthreads=1
        nprocs=2
        nproca=1
        nprocb=2
        nnodes=1
        header="mpipc"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit=""
        ;;
#off#    mpipc_sun)
#off#        site="mpi-m"
#off#        loadmodule="SunStudio/Studio12-200709"
#off#        configureoption="--with-fortran=sun"
#off#        mpiroot="/sw/etch-ia32/mpich2-1.0.8-sun"
#off#        make_command="make -j 2"
#off#        nproma=32
#off#        nthreads=1
#off#        nprocs=2
#off#        nproca=1
#off#        nprocb=2
#off#        nnodes=1
#off#        header="mpipc"
#off#        builddir="bin/echam6"
#off#        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
#off#        submit=""
#off#        ;;

    # Squall
    # ------
    squall | squall_gcc)
        site="mpi-m"
        loadmodule="mpich2/1.2.1p1-gcc"
        configureoption="--with-fortran=gcc"
        mpiroot="/sw/lenny-x64/mpich2-1.2.1p1-gcc"
        make_command="make -j 8"
        nproma=48
        nthreads=1
        nprocs=8
        nproca=2
        nprocb=4
        nnodes=1
        header="squall"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    squall_nag)
        site="mpi-m"
        loadmodule="nag/5.2.747"
        configureoption="--with-fortran=nag"
        mpiroot="/sw/lenny-x64/mpich2-1.2.1p1-static-nag52"
        make_command="make -j 8"
        nproma=48
        nthreads=1
        nprocs=8
        nproca=2
        nprocb=4
        nnodes=1
        header="squall"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    squall_intel)
        site="mpi-m"
        loadmodule="jdk intel/11.1.072 mpich2/1.2.1p1-intel"
        configureoption="--with-fortran=intel"
        mpiroot="/sw/lenny-x64/mpich2-1.2.1p1-intel"
        make_command="make -j 8"
        nproma=48
        nthreads=1
        nprocs=8
        nproca=2
        nprocb=4
        nnodes=1
        header="squall"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    squall_pgi)
        site="mpi-m"
        loadmodule="pgi/10.3 mpich2/1.2.1p1-pgi"
        configureoption="--with-fortran=pgi"
        mpiroot="/sw/lenny-x64/mpich2-1.2.1p1-pgi"
        make_command="make -j 8"
        nproma=48
        nthreads=1
        nprocs=8
        nproca=2
        nprocb=4
        nnodes=1
        header="squall"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    squall_sun)
        site="mpi-m"
        loadmodule="sun/Studio12.1-3 mpich2/1.2.1p1-sun"
        configureoption="--with-fortran=sun"
        mpiroot="/sw/lenny-x64/mpich2-1.2.1p1-sun"
        make_command="make -j 8"
        nproma=48
        nthreads=1
        nprocs=8
        nproca=2
        nprocb=4
        nnodes=1
        header="squall"
        builddir="build/x86_64-unknown-linux-gnu"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;

    # Tornado
    # -------
    tornado | tornado_gcc)
        site="mpi-m"
        loadmodule="GCC/4.3.3"
        configureoption="--with-fortran=gcc"
        mpiroot="/sw/sles10-x64/ofed/openmpi-1.4.0-gcc43"
        make_command="make -j 8"
        nproma=36
        nthreads=1
        nprocs=16
        nproca=2
        nprocb=8
        nnodes=1
        header="tornado"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    tornado_nag)
        site="mpi-m"
        loadmodule="NAG/5.2.721"
        configureoption="--with-fortran=nag"
        mpiroot="/sw/sles10-x64/ofed/openmpi-1.4.0-nag52"
        make_command="make -j 8"
        nproma=36
        nthreads=1
        nprocs=16
        nproca=8
        nprocb=2
        nnodes=1
        header="tornado"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    tornado_intel)
        site="mpi-m"
        loadmodule="INTEL/11.1.072"
        configureoption="--with-fortran=intel"
        mpiroot="/sw/sles10-x64/ofed/openmpi-1.4.0-intel11"
        make_command="make -j 8"
        nproma=36
        nthreads=1
        nprocs=16
        nproca=2
        nprocb=8
        nnodes=1
        header="tornado"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    tornado_pgi)
        site="mpi-m"
        loadmodule="PGI/10.4"
        configureoption="--with-fortran=pgi"
        mpiroot="/sw/sles10-x64/ofed/openmpi-1.4.0-pgi9"
        make_command="make -j 8"
        nproma=36
        nthreads=1
        nprocs=16
        nproca=2
        nprocb=8
        nnodes=1
        header="tornado"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;
    tornado_sun)
        site="mpi-m"
        loadmodule="SUN/Studio12.1-3"
        configureoption="--with-fortran=sun"
        mpiroot="/sw/sles10-x64/ofed/openmpi-1.3.3-sun12"
        make_command="make -j 8"
        nproma=36
        nthreads=1
        nprocs=16
        nproca=2
        nprocb=8
        nnodes=1
        header="tornado"
        builddir="bin/echam6"
        start="$mpiroot/bin/mpiexec -n \${NCPUS}"
        submit="qsub"
        ;;

    # MPIMAC
    # ------
    mpimac | mpimac_gcc)
        site=""
        loadmodule=""
        configureoption="--with-fortran=gcc"
        mpiroot="/opt/local"
        make_command="make -j 2"
        nproma=48
        nthreads=1
        nprocs=2
        nproca=1
        nprocb=2
        nnodes=1
        header="default"
        builddir="bin/echam6"
        start="$mpiroot/bin/openmpiexec -n \${NCPUS}"
        submit=""
        ;;

    # Default (for unspecified target)
    # -------
    default)
        site=""
        loadmodule=""
        configureoption="--with-mpi=no"
        mpiroot=""
        make_command="make -j 1"
        nproma=64
        nthreads=1
        nprocs=1
        nproca=1
        nprocb=1
        nnodes=1
        header="default"
        builddir="bin/echam6"
        start=""
        submit=""
        ;;

    # Error trap, in case $target has unknown value
    *)
        db_status=1
        ;;

esac

if   [ "$db_status" == "0" ]
then
    echo "Parameters set for target=$target"
    echo "  site            = $site"
    echo "  loadmodule      = $loadmodule"
    echo "  configureoption = $configureoption"
    echo "  mpiroot         = $mpiroot"
    echo "  make_command    = $make_command"
    echo "  nproma          = $nproma"
    echo "  nthreads        = $nthreads"
    echo "  nprocs          = $nprocs"
    echo "  nproca          = $nproca"
    echo "  nprocb          = $nprocb"
    echo "  nnodes          = $nnodes"
    echo "  header          = $header"
    echo "  builddir        = $builddir"
    echo "  start           = $start"
    echo "  submit          = $submit"
else
    echo "No parameters set for target=$target --> exit"
    exit $db_status
fi

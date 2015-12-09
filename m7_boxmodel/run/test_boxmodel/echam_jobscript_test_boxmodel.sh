#!/bin/bash
#SBATCH --job-name="test_boxmodel"
#SBATCH --ntasks=1
#SBATCH --time=00:01:00
#SBATCH --output=slurm_test_boxmodel_%j.txt
#SBATCH --error=slurm_test_boxmodel_%j.txt
#SBATCH --account="s235"
#!/bin/bash -l

set -eu 

#-----------------------------------------------------------------------------
# Sylvaine Ferrachat 2011 (after Doris Folini)
#
# Runscript for echam on CSCS Cray XE6 machine (rosa), as of 01.2012 (slurm)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Env Variables:

unset mc
ulimit -s unlimited

export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=536870912
export OMP_NUM_THREADS=1
export MPICH_MAX_SHORT_MSG_SIZE=8000
export MPICH_PTL_UNEX_EVENTS=81920
export MPICH_UNEX_BUFFER_SIZE=150M

#----------------------------------------------------------------------------------------
#--- Start the run:

cd /scratch/rosa/mariaf/test_boxmodel/

set +e
aprun -n 1 ./m7_box

status_echam="$?"
#
if [[ "$status_echam" -ne "0" ]] && [[ "$status_echam" -ne "127" ]] && [[ "$status_echam" -ne "1" ]] ; then
    echo "ERROR: model run stopped with return value ${status_echam}."
    exit
fi

set -e

#----------------------------------------------------------------------------------------
#--- Launch some p-proc job here:

flag_p_proc=true                # flag to launch the p-proc

if $flag_p_proc ; then # this submits a job to be executed on the p-proc machine julier.cscs.ch. 
                       # This job drives the execution of :
                       # /project/s235/Scripts/Pproc/copy_remote.sh.
                       # If this script needs to know more variables than 
                       # 'exp', 'exp_dir' and 'p_proc_dir',
                       # you need to declare them as environment variables before
                       # launching the script

   declare -x exp=test_boxmodel
   declare -x exp_dir=/scratch/rosa/mariaf/test_boxmodel/
   declare -x p_proc_dir=/project/s235/mariaf/test_boxmodel

   sbatch -M julier \
                       --job-name="copy_remote" \
                       --ntasks=1 \
                       --time=03:00:00 \
                       --output=/users/mariaf/m7_boxmodel/grazia/run/test_boxmodel/slurm_copy_remote_%j.txt \
                       --error=/users/mariaf/m7_boxmodel/grazia/run/test_boxmodel/slurm_copy_remote_%j.txt \
                       --account="s235" \
                       --export=ALL \
                             /project/s235/Scripts/Pproc/copy_remote.sh
fi

#----------------------------------------------------------------------------------------
#--- Tar the rerun files:

#>>SF uncomment the following line if the path to ncdump is not none none by default
#     and if netcdf is a valid module of your system (else, you may hardcode the path to ncdump)
module load netcdf   # to get the path to ncdump. Note: still safe if netcdf is already loaded 
#<<SF

if [ -e rerun_test_boxmodel_echam ] ; then
   rerun_date=`ncdump -h rerun_test_boxmodel_echam | grep vdate | cut -c12-19`
   rerun_tar=rerun_test_boxmodel_${rerun_date}.tar

   tar cvf $rerun_tar rerun_test_boxmodel_[a-z]*
else
   echo "No rerun file was produced: Don't do any job chaining and stop execution here"
   exit # no need to prepare a potential namelist file for a next submission if there's no rerun file
fi

#----------------------------------------------------------------------------------------
#--- Adjust the rerun flag in the namelist for any subsequent job:

\mv namelist.echam namelist.echam.bak
cat namelist.echam.bak | sed -e 's/^ *[lL][rR][eE][sS][uU][mM][eE].*$/lresume=.true./' > namelist.echam

#----------------------------------------------------------------------------------------
#--- Submit the next job:

   if [[ "$status_echam" -eq "0" ]] ; then
      cd /users/mariaf/m7_boxmodel/grazia/run/test_boxmodel
      sbatch /users/mariaf/m7_boxmodel/grazia/run/test_boxmodel/echam_jobscript_test_boxmodel.sh
   fi


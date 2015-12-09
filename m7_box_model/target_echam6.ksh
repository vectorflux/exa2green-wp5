	#!/bin/ksh
#==============================================================================
#
# This script makes the run scripts for the specified target 
# and experiment. Target and experiment namelist are given as arguments:
#
#   Argument 1 = $1 = target name
#                     the target must be defined in target_database.ksh
#   Argument 2 = $2 = experiment name, for which a descriptor file with 
#                     prefix "exp" must exist in run/, e.g. "exp.iconrun"
#
# This involves the following steps:
#
# 1. read in target database values for specified target
#
# 2. concatenate script segments:
#    - target specific header file
#    - experiment specific namelist
#    - execution part, preparing and starting the run
#
# 3. substitute generic variables in the concatenated script by 
#    the actual target specific values
#
# Marco Giorgetta, MPI-M, 2010-04-21
#
#==============================================================================
#==============================================================================

# Setting of default values

#==============================================================================
#==============================================================================

target=NotSet
expname=NotSet
exp_id=NotSet
if [ -e ${HOME}/.acct ]
then
  acc=`cat ${HOME}/.acct`
else
  acc=NotSet
fi

#==============================================================================
#==============================================================================

# define of functions

#==============================================================================
#==============================================================================

show_help_info ()
{
  more target_echam6.info 
}

#------------------------------------------------------------------------------

analyze_para ()
{
  paras=`echo $@`
  for para in $paras
  do
    p=`echo $para | cut -d '-' -f2 | cut -d '=' -f1`
    v=`echo $para | cut -d '=' -f2`
#    echo "p=$p v=$v"
    case ${p} in
      exp_id)
        exp_id="$v"
        ;;
      account)
        acc="$v"
        ;;
      exp)
        expname="$v"
        ;;
      target)
        target="$v"
        source="argument"
        ;;
     expname)
        show_help_info
        exit 0
        ;;
      list)
        echo "Possible experiments are:"
        cd run;ls -l exp.* | awk '{printf("    %s\n",$8)}' ; cd ..
        exit 0 
        ;;
      h)
        show_help_info
        exit 0
        ;;
      *)
        echo " "
        echo " "
        echo " Wrong Parameter(s) $@ "
        echo " "
        echo " "

	show_help_info
        exit 1
      esac
    done


}
#==============================================================================
#==============================================================================

# Main part of script

#==============================================================================
#==============================================================================

# Executable part of script
# -------------------------

scriptname="target_echam6.ksh"

#------------------------------------------------------------------------------

# Set target variable, which is used to select case specific parameters from a
# built in data base.

#if   [ "$#" == "2" ]         # check if 2 arguments are given
#then
#
#    target="$1"
#    expname="$2"
#
#else                         # --> as default
#    echo "  $scriptname needs 2 arguments:"
#    echo "    $scriptname <target name> <experiment name>"
#    exit 1
#fi

if   [ "$#" == "0" ]
then
  show_help_info
  exit 1
else
  analyze_para $@
fi

if   [ "$expname" == "NotSet" -o "$target" == "NotSet" ]
then
  show_help_info
  exit 1
fi


echo "$scriptname: Start"

echo "$scriptname:   Building run script for:"
echo "$scriptname:     target name     = $target"
echo "$scriptname:     experiment name = $expname"

#------------------------------------------------------------------------------

# Get target specific parameters from database
#
# db_status = exit status of database, 0=ok, 1=parameters not known

. ./target_functions.ksh
. ./target_database.ksh

#------------------------------------------------------------------------------

# Concatenate iconrun script

runscript=$expname.$target

cd run

cat head.$header  >  $runscript.in
cat exp.$expname  >> $runscript.in

echo "#------------------------------------------------------------------------------
# The next few lines has to be there because for loading the special modules
# we have to know which machine and compiler is used." >> $runscript.in

echo " " >> $runscript.in
echo "target=$target" >> $runscript.in
echo " " >> $runscript.in
echo ". ../target_database.ksh" >> $runscript.in
echo ". ../target_functions.ksh" >> $runscript.in
echo "#------------------------------------------------------------------------------" >> $runscript.in


# load module for compiler, if defined

if [ "$loadmodule" != "" ] 
then
  echo "# load module for compiler." >> $runscript.in
  echo " " >> $runscript.in
  echo "module load \$loadmodule " >> $runscript.in
  echo " " >> $runscript.in

fi

if [ "$header" == "tornado" ] 
then
   echo "LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/sw/sles10-x64/szip-2.1/lib" >> $runscript.in
fi

if [ "$header" == "squall" ] 
then
  case ${target} in

    # Blizzard
    # --------
    squall_gcc)
      echo "LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/sw/lenny-x64/netcdf-4.0.1-gcc/lib" >> $runscript.in
      ;;
    squall_nag)
      echo "LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/sw/lenny-x64/netcdf-4.0.1-nag/lib" >> $runscript.in
      ;;
    squall_intel)
      echo "LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/sw/lenny-x64/netcdf-4.0.1-intel/lib" >> $runscript.in
      ;;
    squall_pgi)
      echo "LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/sw/lenny-x64/netcdf-4.0.1-pgi/lib" >> $runscript.in
      ;;
    squall_sun)
      echo "LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:/sw/lenny-x64/netcdf-4.0.1-sun/lib" >> $runscript.in
      ;;
  esac

fi
cat exec.echam6run  >> $runscript.in

#------------------------------------------------------------------------------

# Process the concatenated run script. Replace template variables by the 
# values of the target specific variables

# Begin NEW
sed -i s:'${EXP}':"$expname":g       $runscript.in
sed -i s:'${MPIROOT}':"$mpiroot":g       $runscript.in
sed -i s:'${NPROMA}':"$nproma":g         $runscript.in
sed -i s:'${BIN_DIR}':"$builddir":g      $runscript.in
sed -i s:'${START}':"$start":g           $runscript.in 
sed -i s:'${EXPNAME}':"$expname":g       $runscript.in

sed -i s:'${NPROCS}':"$nprocs":g         $runscript.in
sed -i s:'${NCPUS}':"$nprocs":g          $runscript.in
sed -i s:'${NPROCA}':"$nproca":g         $runscript.in
sed -i s:'${NPROCB}':"$nprocb":g         $runscript.in
sed -i s:'${NNODES}':"$nnodes":g         $runscript.in
sed -i s:'${NTHREADS}':"$nthreads":g     $runscript.in

sed -i s:'${SUBMIT}':"$submit":g         $runscript.in
sed -i s:'${RUNSCRIPT}':"$runscript":g   $runscript.in

if [ "${exp_id}" != "NotSet" ]
then
  sed -i s:'${EXP_ID}':"${exp_id}":g   $runscript.in
else
  sed -i s:'${EXP_ID}':"bb_run":g   $runscript.in
fi

if [ "${acc}" != "NotSet" ]
then
  sed -i s:'${ACCOUNT}':"$acc":g   $runscript.in
else
  sed -i s:'${ACCOUNT}':"default":g   $runscript.in
fi

# End NEW

if [ "${exp_id}" != "NotSet" ]
then
  out_file=${expname}_${exp_id}.${target}
else
  out_file=${expname}.${target}
fi

mv $runscript.in ${out_file}
chmod 755 $out_file

#------------------------------------------------------------------------------

# Start script
echo "$scriptname:   Generated runscript run/${out_file}"

#==============================================================================

# Finish script with status OK

echo "$scriptname: End"
exit 0

#!/bin/ksh
#==============================================================================
#
# This script configures and makes the ICON executables for a specific target. 
#
# The executables are made for the build target given by 
# the variable $target. Its value is determined as follows:
#
#   if the target name is given as argument, then:
#     target=$1
#   else:
#     target="default" 
#
# If the value of $target exists in the target database, then variables 
# used for the configuration are set, and the script proceeds with the 
# configure&make process.
#
# If the value of $target does not exist in the case table, 
# then the script stops.
#
# Marco Giorgetta, MPI-M, 2010-04-12
#
#==============================================================================


# Executable part of script
# -------------------------

scriptname="target_confmake.ksh"

echo "$scriptname: start"

#------------------------------------------------------------------------------

# Set target variable, which is used to select case specific parameters from a
# built in data base.

if   [ "$1" != "" ]          # --> from argument
then
    target="$1"
else                         # --> as default
    target="default"
fi

echo "$scriptname: target = $target"


# Get target specific parameters from database
#
#   loadmodule      = name of Fortran compiler module to be loaded
#   configureoption = option(s) for configure command
#
# db_status = exit status of database, 0=ok, 1=parameters not known

. ./target_database.ksh

#------------------------------------------------------------------------------
# Define functions
# ----------------

# "Include" functions from external file
# - check_error()
# - return_ok()
# - exec_and_check()

. ./target_functions.ksh

#==============================================================================

#------------------------------------------------------------------------------

# load module for compiler, if defined

if [ "$loadmodule" != "" ] 
then
    module load $loadmodule
fi

#------------------------------------------------------------------------------

# Configure Makefile ...
exec_and_check "./configure $configureoption"

# ... and make executables
exec_and_check "make clean"
#exec_and_check "make"
$make_command
error_status=$?

if [ "${STATUS_FILE}" != "" ]
then
  echo "$error_status" > ${STATUS_FILE}
fi

if [ $error_status != 0 ]
then
  echo "check_error()"
  echo "   ERROR : $make_command"
  exit $error_status
fi

#==============================================================================

# Finish script with status OK

return_ok "$scriptname: end"

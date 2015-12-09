#!/bin/ksh
#==============================================================================
#
# Define useful functions to execute commands and check the exit status.
#
# Walter Sauf,     MPI-M, 2009-10-22
# Marco Giorgetta, MPI-M, 2010-04-12
#
#==============================================================================

check_error()
{

    # Check if the first parameter (return status) is not OK
    #
    # Arguments:
    #   $1 = error status: 0 = OK, not 0 = ERROR
    #   $2 = error message

    if [ "${STATUS_FILE}" != "" ] 
    then
        echo "$1" > ${STATUS_FILE}
    fi

    if [ $1 != 0 ] 
    then

	echo "check_error()"
	echo "   ERROR : $2"

	exit $1

    fi

}

#------------------------------------------------------------------------------

return_ok()
{

    # Exit with status = 0 = OK
    #
    # Arguments:
    #   $1 =  message

    echo "return_ok()"
    echo "$1"

    exit 0

}

#------------------------------------------------------------------------------

exec_and_check()
{

    # Execute command and check for error
    #
    # Arguments:
    #   $1 = command to be executed

    $1

    check_error $? "exec_and_check(): $1"

}

#==============================================================================
#
# Function  check_setting_file
#
#==============================================================================

check_setting_file()
{
  if [ -a ../setting ]
  then
    echo "Load Setting"
    . ../setting
    return
  else
    STATUS_FILE=/dev/null
  fi
}

#==============================================================================
#
# loads the correct system profile on each computer
#
#==============================================================================
load_system_profile()
{
sys=`uname -s`

echo "load_system_profile"
echo "  site   = ${site}"
echo "  system = ${sys}"

case ${sys} in
    AIX)
        case  ${site} in
            dkrz)
                echo "  load /client/etc/profile.blizzard"
                . /client/etc/profile.blizzard
                ;;
        esac
        ;;
    Linux)
        case ${site} in
            mpi-m)
                echo "  load /client/etc/profile.zmaw"
                . /client/etc/profile.zmaw
                ;;
            dwd)
                echo "  nothing to be done"
                ;;
        esac
        ;;
esac
}

load_system_profile


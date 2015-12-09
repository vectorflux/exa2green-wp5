#!/bin/bash

#--------------------------------------------------------------------------------------------------
#
# Sylvaine Ferrachat 2012-02
#
# This script parses relevant information from the echam namelists to 
# find out which input files have to be used (e.g. time-dep ozone, nudging files, etc..)
#
#--------------------------------------------------------------------------------------------------

echo "----" | tee -a $log_file
echo "Parsing namelist_${exp}.echam to get input files-related flags..." | tee -a $log_file

#-------------------------------------------------------------------------------------
#-- Sea surface temperature and sea ice coverage:

    #-- parse from namelist (lamip)
        flag_time_dep_sst_sic=$(parse_fortran_logical lamip ${script_dir}/namelist_${exp}.echam)

    #-- security in case 'lamip' was not found in namelist
        if [ -z $flag_time_dep_sst_sic ] ; then # flag_time_dep_sst_sic is non-assigned
           flag_time_dep_sst_sic=false          # this reproduces the default in echam6
        fi

    #-- set sst_sic_dataset in case it is not defined
        if [ -z $sst_sic_dataset ] ; then
           sst_sic_dataset="amip2"
        fi

#-------------------------------------------------------------------------------------
#-- Ozone:

    #-- parse from namelist (io3)
        io3=`sed -n -e 's/^ *[i|I][o|O]3 *= *\([0-9]*\).*/\1/p' \
                 ${script_dir}/namelist_${exp}.echam`
 
    #-- security in case io3 was not found in namelist
        if [ -z $io3 ] ; then
           io3=3
        fi

    #-- set flag_CMIP5_ozon (echam6 and echam6-hammoz)
        if [[ "$io3" -eq "4" ]] ; then # CMIP5 ozone
           flag_CMIP5_ozon=true
        else # this also applies if io3 is not defined,
             # which reproduces the default in echam6
           flag_CMIP5_ozon=false
        fi

#-------------------------------------------------------------------------------------
#-- Nudging:

    #-- parse from namelist (lnudge)
        flag_nudg=$(parse_fortran_logical lnudge ${script_dir}/namelist_${exp}.echam)

    #-- security in case 'lnudge' was not found in namelist
        if [ -z $flag_nudg ] ; then
           flag_nudg=false
        fi

    #-- Additional check: removes nproma from namelist if lnudge = .true.
        if $flag_nudg ; then
           echo
           echo "You're running in nudged mode, removing any remaining nproma definition in "\
"${script_dir}/namelist_${exp}.echam..."
           sed -i".bak" \
               -e '/^ *[n|N][p|P][r|R][o|O][m|M][a|A] *=/d' \
               ${script_dir}/namelist_${exp}.echam
        fi

#-------------------------------------------------------------------------------------
#-- Time-dep solar irradiance 

    #-- parse from namelist (isolrad)
        isolrad=`sed -n -e 's/^ *[i|I][s|S][o|O][l|L][r|R][a|A][d|D] *= *\([0-9]*\).*/\1/p' \
                     ${script_dir}/namelist_${exp}.echam`
    
    #-- security in case 'isolrad' was not found in namelist
        if [ -z $isolrad ] ; then
           isolrad=3  # default in echam6
        fi  
    
    #-- set flag_time_dep_sol_irr
        if [[ "$isolrad" -eq "1" ]] ; then # time-dep read from file
           flag_time_dep_sol_irr=true
        else
           flag_time_dep_sol_irr=false
        fi

#-------------------------------------------------------------------------------------
#-- aerosols for radiation and nudging data format

    #-- parse from namelist (iaero)
        iaero=`sed -n -e 's/^ *[i|I][a|A][e|E][r|R][o|O] *= *\([0-9]*\).*/\1/p' \
                     ${script_dir}/namelist_${exp}.echam`

    #-- security in case iaero was not found in namelist
        #SF note: the script will stop if iaero is not found. In echam6, default iaero is 2
        #         but I don't want to set it here to 2, since iaero=2 is obsolete

        if [ -z $iaero ] ; then
           echo "Error! iaero does not seem to be defined in ${script_dir}/namelist_${exp}.echam." \
                | tee -a ${log_file}
           echo "Exiting!"
           exit 1
        fi

     #-- set flag_kinne_aerosols, flag_stenchikov_aerosols, flag_crowley_aerosols, flag_hamclimatology_aerosols
         flag_kinne_aerosols=false
         flag_stenchikov_aerosols=false
         flag_crowley_aerosols=false
         flag_submclim_aerosols=false

         case $iaero in
            3)
              flag_kinne_aerosols=true
            ;;
            5)
              flag_kinne_aerosols=true
              flag_stenchikov_aerosols=true
            ;;
            6)
              flag_kinne_aerosols=true
              flag_stenchikov_aerosols=true
              flag_submclim_aerosols=true
            ;;
            7)
              flag_kinne_aerosols=true
              flag_crowley_aerosols=true
            ;;               
         esac

     #-- parse from namelist (inudgformat)
         inudgformat=`sed -n -e 's/^ *[i|I][n|N][u|U][d|D][g|G][f|F][o|O][r|R][m|M][a|A][t|T] *= *\([0-9]*\).*/\1/p' \
                     ${script_dir}/namelist_${exp}.echam`

     #-- set flag_nudg_netcdf
         if [[ "inudgformat" -eq "2" ]] ; then
            flag_nudg_netcdf=true
         else
            flag_nudg_netcdf=false
         fi

#-------------------------------------------------------------------------------------
#-- function to check flags, that will be used in the symlink script

    function flag_check()
    {
   
    case_insensitive=$1
    flag_name_string=$2
    flag_name_var=\$"$flag_name_string"
    flag_to_test=`eval "expr \"$flag_name_var\" "`

    nvalues=0
    declare -a flag_values
    until [ -z "$3" ] ; do
        flag_values[$nvalues]="$3"
        shift
        ((nvalues=$nvalues+1))  
    done

    if [[ "$nvalues" -eq 0 ]] ; then
       echo "Error! No possible values for $flag_name_string are defined!" | tee -a $log_file
       echo "Exiting" | tee -a $log_file
       exit 1
    fi
    
    #-- Converts to lower case when relevant
        if $case_insensitive ; then
           flag_to_test=`echo $flag_to_test | tr '[:upper:]' '[:lower:]'`
           eval "$flag_name_string=$flag_to_test" # re-assign new value to original flag
           case_string="case insensitive"
        else
           case_string="case sensitive"
        fi
    
    #-- Check if flag_to_test is among ${flag_values[*]}
        found=false
    
        for ((ival=0;ival<$nvalues;ival++)) ; do
            if [[ "$flag_to_test" == ${flag_values[$ival]} ]] ; then
               found=true
            fi
        done
    
        if ! $found ; then
           echo "Wrong value for $flag_name_string ($flag_to_test)!" | tee -a $log_file
           echo "It must be one of those ($case_string):" "${flag_values[*]}" | tee -a $log_file
           echo "Exiting" | tee -a $log_file
           exit 1
        fi
    }

#-------------------------------------------------------------------------------------
#-- end (normal)

    echo | tee -a $log_file

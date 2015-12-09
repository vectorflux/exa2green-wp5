#!/bin/bash

#--------------------------------------------------------------------------------------------------
#
# Sylvaine Ferrachat 2012-01
#
# This scripts parses the user's setting file to set the starting year / starting month / stopping year / stopping month
# (start_year, stop_year, start_month, stop_month) of the run. 
#
# In case of a rerun, the starting date corresponds to the real date in the corresponding 
# rerun_${exp}_echam file, and not to the whole experiment's starting date.
#
# Based on start_year, stop_year, start_month, stop_month, this script also computes the following annex variables:
#
#   - the total number of months              --> nmonths
#   - starting date as YYYYMM                 --> start_yyyymm
#   - stopping date as YYYYMM                 --> stop_yyyymm
#   - year before starting year               --> start_year_m1
#   - year after stopping year                --> stop_year_p1
#   - 1 month before starting date as YYYYMM  --> start_yyyymm_m1
#   - 1 month after stopping date  as YYYYMM  --> stop_yyyymm_p1
#
# These annex vars are needed at input file linking stage and / or at job chaining stage
#
#--------------------------------------------------------------------------------------------------

echo "----" | tee -a $log_file
echo "Setting starting and stopping dates of the current run..." | tee -a $log_file

#-------------------------------------------------------------------------------------
#-- Set start year and month of this run

    IFS=',' read -ra date_start_array <<<"$date_start"

    exp_start_year=`printf '%04d' ${date_start_array[0]}`  # starting year of the whole echam experiment
                                                           # (ensures strict 4-digits formatting 
                                                           # with '0' left padding )
    exp_start_month=`printf '%02d' ${date_start_array[1]}` # starting month of the whole echam experiment
                                                           # (ensures strict 2-digits formatting 
                                                           # with '0' left padding )

    lresume=$(parse_fortran_logical lresume ${script_dir}/namelist_${exp}.echam)
 
    if [ -z $lresume ] ; then #security, if lresume is not defined
       lresume=false
    fi
 
    if ! $lresume ; then # fresh start

       start_year=${exp_start_year}      
       start_month=${exp_start_month}

    else # case of a rerun, must go fetch the previous stopping date in the rerun file
         # and computes the current starting date only (works when 1 cycle last 1 month!)

       exit_stat=`which ncdump &> /dev/null ; echo $?`
       if [[ "$exit_stat" != 0 ]] ; then # ncdump not found!
          echo "Did not find ncdump! Please fix your path (e.g. module load netcdf)." | tee -a ${log_file}
          echo "Exiting"
          exit 1
       fi

       rerun_file=${exp_dir}/rerun_${exp}_echam

       echo | tee -a ${log_file}
       echo "Fetching exact starting date from rerun file ${rerun_file}..." | tee -a ${log_file}

       rerun_date=`ncdump -h ${rerun_file} | grep vdate | cut -c12-19`

       if [[ "$rerun_date" != [0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9] ]] ; then
          echo "Error! The rerun date could not be extracted from ${rerun_file}!" | tee -a $log_file
          echo "Exiting"
          exit 1
       fi

       previous_stop_year=${rerun_date:0:4}
       previous_stop_month=${rerun_date:4:2}

       if [[ "$previous_stop_month" != "12" ]] ; then # normal case (not December)
          start_year=$previous_stop_year
          start_month=`echo "$previous_stop_month + 1" | bc -l`
          start_month=`printf '%02d' $start_month`
       else # handles the December case
          start_year=`echo "$previous_stop_year + 1" | bc -l`
          start_month=01
       fi

       echo "Starting date for this run will be: $start_year$start_month" | tee -a ${log_file}
       echo "INFO: the starting year and month of the whole echam experiment are kept to:\
 $exp_start_year$exp_start_month" | tee -a ${log_file}

    fi

#-------------------------------------------------------------------------------------
#-- Set stop year and month

    IFS=',' read -ra date_stop_array <<<"$date_stop"

    stop_year=`printf '%04d' ${date_stop_array[0]}`  # ensures strict 4-digits formatting (0 padding)
    stop_month=`printf '%02d' ${date_stop_array[1]}` # ensures strict 2-digits formatting (0 padding)

    #-- correct stop_month and stop_year in case the final date is on the 
    #   first day of month at 00:00 (so that the total number of months
    #   and other dependent variables are set correctly)

    stop_day=`echo "${date_stop_array[2]}" | bc -l`  # use of 'bc -l' to convert string to integer
    stop_hour=`echo "${date_stop_array[3]}" | bc -l` # use of 'bc -l' to convert string to integer
    stop_min=`echo "${date_stop_array[4]}" | bc -l`  # use of 'bc -l' to convert string to integer
    stop_sec=`echo "${date_stop_array[5]}" | bc -l`  # use of 'bc -l' to convert string to integer

    if [[ "$stop_day"  -eq "1" ]] && \
       [[ "$stop_hour" -eq "0" ]] && \
       [[ "$stop_min"  -eq "0" ]] && \
       [[ "$stop_sec"  -eq "0" ]] ; then # stop date is XXXX,XX,01,0,0,0 therefore remove 1 month

       if [[ "$stop_month" != "01" ]] ; then #normal case (not January)
          stop_month=`echo "$stop_month-1" | bc -l`
          stop_month=`printf '%02d' $stop_month`
       else # handles the January case
          stop_month=12
          stop_year=`echo "$stop_year-1" | bc -l`
       fi
        
    fi
#-------------------------------------------------------------------------------------
#-- Sanity check

    start_yyyymm=$start_year$start_month
    stop_yyyymm=$stop_year$stop_month
    if [[ "$start_yyyymm" -gt "$stop_yyyymm" ]] ; then
       echo "Error! Starting date is older than stop date!" | tee -a $log_file
       echo "Start date: $start_yyyymm"
       echo "Stop date: $stop_yyyymm"
       echo "Exiting" | tee -a $log_file
       exit 1
    fi

#-------------------------------------------------------------------------------------
#-- Annex variables

    #-- Total number of months
        nmonths=`echo "($stop_year-$start_year)*12+$stop_month-$start_month+1" | bc -l`
        echo "Total number of months for this run:" $nmonths | tee -a $log_file

    #-- Year before starting year
        start_year_m1=`echo "$start_year-1" | bc -l`

    #-- Year after stopping year
        stop_year_p1=`echo "$stop_year+1" | bc -l`

    #-- 1 month before starting date as YYYYMM
        if [[ "$start_month" != "01" ]] ; then # normal case (not January)
           start_yyyymm_m1=`echo "$start_yyyymm-1" | bc -l`
        else # handles the January case
           start_yyyymm_m1=${start_year_m1}"12"
        fi

    #-- 1 month after stopping date  as YYYYMM
        if [[ "$stop_month" != "12" ]] ; then # normal case (not December)
           stop_yyyymm_p1=`echo "$stop_yyyymm+1" | bc -l`
        else # handles the December case
           stop_yyyymm_p1=${stop_year_p1}"01"
        fi

#-------------------------------------------------------------------------------------
#-- Bugfix for nudging:
#   it turns out that ALL nudging files must be present anytime, even if the experiment
#   is a rerun (I an: in this latter case, it would seem logical that the nudging
#   data from previous run is not necessary, but if done so, the code crashes. It
#   implies that the nudging file linking must be done for ALL dates of thw whole experiment
#   which is not the case for other files. It implies that the following additional code:)

    exp_start_yyyymm=$exp_start_year$exp_start_month

    #-- 1 month before experiment starting date as YYYYMM
        if [[ "$exp_start_month" -ne "1" ]] ; then # normal case (not January)
           ((exp_start_yyyymm_m1=$exp_start_yyyymm-1))
        else # handles the January case
           ((exp_start_year_m1=$exp_start_year-1))
           exp_start_yyyymm_m1=${exp_start_year_m1}"12"
        fi

#-------------------------------------------------------------------------------------
#-- end (normal)

    echo | tee -a $log_file

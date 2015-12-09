# @ shell                = /bin/bash
# @ job_name             = backup_runscript_echam6-hammoz 
# @ wall_clock_limit     = 00:02:00
# @ job_type             = parallel
# @ node_usage           = not_shared
# @ network.MPI          = sn_all,not_shared,us
# @ rset                 = rset_mcm_affinity
# @ mcm_affinity_options = mcm_distribute
# @ resources            = ConsumableMemory(750Mb)
# @ node                 = 2
# @ tasks_per_node       = 32
# @ task_affinity        = core(1)
# @ output               = $(job_name).o$(jobid)
# @ error                = $(output)
# @ notification         = error
# @ environment          = COPY_ALL
# @ account_no           = bm0702
# @ class                = express
# @
# @ queue

#!/bin/bash -u

export MP_INFOLEVEL=0
export MP_PRINTENV=no
export MP_LABELIO=no
export MP_SHARED_MEMORY=yes
export MP_SHM_ATTACH_THRESH=256000
export MP_USE_BULK_XFER=yes
export MP_BULK_MIN_MSG_SIZE=128k
export MP_RDMA_MTU=4k
export MP_EAGER_LIMIT=64k
export MP_BUFFER_MEM=32m,512m
export MP_FIFO_MTU=4k
export MP_RFIFO_SIZE=16m
export MP_SINGLE_THREAD=no
export OMP_STACKSIZE=64M
export OMP_NUM_THREADS=1
export XLFRTEOPTS="intrinthds=1:err_recovery=no:langlvl=2003std"

ulimit -c 0 

#------------------------------------------------------------------------------------------------------
#-- For job chaining:

set +e
if [ -z $first_launch ] ; then # ie: if 'first_launch' is not already defined
                               #
                               # this var is used as a memory for the script to know whether this is
                               # a first job launch from user (first_launch not defined or 'true') 
                               # or a job from job chaining (first_launch=false)
                               # This should not be confused with the following 'init_restart_flag' variable
                               # which determines whether this is an echam rerun or a start from scratch
                               # A first launch (first_launch undefined or 'true') can be made with
                               # init_restart_flag=.true., when the user launches a run as a restart
                               #
                               # WARNING!
                               # The user should not touch this var. Its value is always handled automatically
                               # by the script. At job chaining, the next job wil be launched with setting
                               # first_launch=false
   declare -x first_launch=true
fi

if [ $first_launch ] ; then
   echo "This is the first launch of this job chain."
else
   echo "This is not the first launch of this job chain (resuming echam)."
fi

set -e

#------------------------------------------------------------------------------------------------------
#-- General settings:

model_dir="/home/zmaw/m222045/echam/echam6-hammoz-trunk"      # location of the sources and bin of the model
exp="backup_runscript_echam6-hammoz"                          # experiment identifier
script_dir=${model_dir}/run/${exp}                         # where the jobscript and exp namelist are to be found
this_script=${script_dir}/${exp}.sh                           # name of this script
log_file=${script_dir}/log_${exp}_`date +%Y-%m-%d_%H.%M`.txt  # for logging the file linking stage run

#------------------------------------------------------------------------------------------------------
#-- Compute settings for machine: blizzard (IBM Power6, Blizzard, DKRZ)

account="bm0702"                        # project ID for accounting 
exp_dir="/work/$account/$USER/${exp}/"  # where the run will be done 
nproma=48                               # vector length (don't modify if you don't know about it)
nproca=16                               # number of processors 'A'
nprocb=4                                # number of processors 'B'

ncpus=$(($nproca*$nprocb))              # total number of cpus

#------------------------------------------------------------------------------------------------------
#-- ECHAM6-HAMMOZ settings:

   if $first_launch ; then # do this only when this is a first launch

      #-- General:

      model_type="echam6-hammoz"                        # model type (echam5.5-ham, echam6 or echam6-hammoz)
      model_bin=${model_dir}/bin/echam6                 # model bin name
      emissions_spec=${script_dir}/emi_spec_${exp}.txt  # name of your exp emi_spec file to copy to $exp_dir
      
      hres="T63"                    # horizontal spectral truncation
      vres="L31"                    # vertical resolution
      oceres="GR15"                 # resolution of the ocean model used to produce the land-sea mask
      ntiles=11                     # number of tiles in the vegetation model
      
      no_cycles=15                   # number of cycles in one integration (1 cycle = timespan between 2 'putrerun' events)
      init_restart_flag=.false.     # echam restart flag at launch of this job 
                                    # (in case of job chaining, the proper rerun flag will be automatically 
                                    #  set when further jobs will be started)
      
      date_start="1999,10,01,0,0,0" # start of the experiment - Should never be modified when making a rerun
      date_stop="2001,01,01,0,0,0"  # end of the experiment - Can be freely adjusted during the whole experiment
      
      input_files_version="v01_001" # version label for input files !!do not change if you don't know about it!!
      scenario="RCP45"              # in case it is relevant for the given setup (using either variable GHG's
                                    # ozone or climatologic aerosols, AND the experiment's time range 
                                    # overlaps with [2009,2100]), choose here the scenario you'd like
                                    # (RCP45, XXX, all case insensitive)
      aero_dataset=aerocom_II    # aerocom or aerocom_II (case sensitive)
   
      #-- ECHAM6-HAMMOZ namelists:
   
          #-- ECHAM:
   
          cat > ${script_dir}/namelist_${exp}.echam << EOF
&PARCTL
 nproca = $nproca
 nprocb = $nprocb
/
&RUNCTL 
 nproma         = ${nproma}
 lresume        = $init_restart_flag,
 lmidatm        = .false.
 out_datapath   = "$exp_dir"
 out_expname    = "$exp"
 out_filetype   = 2     ! 1--> grib; 2--> NetCDF; 6--> NetCDF4;
 trac_filetype  = 2     ! 1--> grib; 2--> NetCDF;
 rerun_filetype = 2     ! 2--> NetCDF; 4--> NetCDF2
 dt_start       = $date_start
 dt_stop        = $date_stop
 putdata        = 1,'months','last',0
 trigfiles      = 1, 'months', 'first', 0
 putrerun       = 1,'months','last',0
 no_cycles      = ${no_cycles}, 
 lamip          = .false.
 lnudge         = .false.
 lipcc          = .false.
 ldebugs        = .false.
 ltimer         = .false.
/
&SUBMODELCTL
 lmegan        = .false.
 lmethox       = .true.
 lham          = .true.
 lmoz          = .false.
 lhammoz       = .false.
 emi_basepath  = "/pool/data/ECHAM6-HAMMOZ/${input_files_version}/emissions_aerocom_II/"
/
&SUBMDIAGCTL
 vphysc_lpost     = .true.
 vphyscnam        = 'all'
 wetdep_lpost     = .true. 
 wetdepnam        = 'all' 
 wetdep_gastrac   = 'SO2', 'SO4_gas'
 wetdep_keytype   = 1 
 drydep_lpost     = .true. 
 drydepnam        = 'all' 
 drydep_gastrac   = 'SO2', 'SO4_gas'
 drydep_keytype   = 1 
 sedi_lpost       = .true. 
 sedinam          = 'all' 
 sedi_keytype     = 1 
 emi_lpost        = .true.
 emi_lpost_sector = .true.
 eminam           = 'all'
 emi_gastrac      = 'all'
/
&DEBUGSCTL
 putdebug_stream = 1, 'days', 'last', 0
/
&RADCTL
 iaero   = 1     ! 1 for interactive (lham=true), 2 for Tanre 3 for Kinne  
 l_srtm  = .true.
 l_lrtm  = .true.
 isolrad = 3      
 io3     = 4     ! 3 --> obsolete! climatology from IPCC-NetCDF file, 4--> CMIP5 new data
/
&PHYSCTL
 lcover         = .false.
 lcdnc_progn    = .true.
 ncd_activ      = 1
 nic_cirrus     = 2
 nauto          = 2
/
&HAM_DUSTCTL
 ndustE2 = 4
/
&HAMCTL
 lsoa          = .false.
 nseasalt      = 5
 nwetdep       = 2
 naerorad      = 1
 nrad          = 0, 3, 3, 3, 3, 3, 3
 nradmix       = 0, 1, 1, 1, 1, 1, 1  
 nraddiag      = 1
/
&NDGCTL
 inudgformat     = 2
 dt_nudg_start   = $date_start
 dt_nudg_stop    = $date_stop
 lnudgdbx        = .false.
 lnudgini        = .true.
 nudglmin        = 1
 nudglmax        = ${vres#L}
 nudgsmin        = 0
 nudgsmax        = ${hres#T}
 lnudgimp        = .TRUE.
 nudgd           = ${vres#L}*0.579
 nudgt           = ${vres#L}*1.16
 nudgv           = ${vres#L}*4.63
 nudgp           = 1.16
 ltintlin        = .false.
 ldamplin        = .true.
 nudgdamp        = 1.
 nudgdsize       = 0.5
 ndg_file_nc    = "ndg%y4%m2.nc"
/
EOF

          #-- JS-BACH:

          cat > ${script_dir}/namelist_${exp}.jsbach <<EOF
&JSBACH_CTL
  standalone = .false.
  ! --- number of tiles ---
  ntiles = ${ntiles}

  ! --- options to activate the different jsbach modules ---
  use_bethy = .true.
  use_phenology = .true.
  use_albedo = .true.
  use_dynveg = .false.

  ! --- output options ---
  file_type = "NETCDF"
  lpost_echam = .false.
  debug = .false.
/
&ALBEDO_CTL
  use_albedocanopy = .false.
/
&CBALANCE_CTL
  read_cpools = .false.
/
&DYNVEG_CTL
  read_fpc = .false.
  dynveg_feedback = .false.
/
&CLIMBUF_CTL
  init_running_means = .false.
/
EOF

          #-- Mean-values streams:

#SF uncomment the following if you need to compute online monthly means:

#cat >> ${script_dir}/namelist_${exp}.echam << EOF
#&MVSTREAMCTL
#  m_stream_name = 'ham', 'rad', 'tracer', 'vphysc', 'activ' 
#/
#EOF

cat > ${script_dir}/ham.nml <<EOF
&MVCTL
  PUTMEAN = 1,'months','last',0
  meannam = 'all'
/
EOF

cat > ${script_dir}/rad.nml <<EOF
&MVCTL
  PUTMEAN = 1,'months','last',0
  meannam = 'all'
/
EOF

cat > ${script_dir}/tracer.nml <<EOF
&MVCTL
  PUTMEAN = 1,'months','last',0
  meannam = 'all' 
/
EOF

cat > ${script_dir}/vphysc.nml <<EOF
&MVCTL
  PUTMEAN = 1,'months','last',0
  meannam = 'all'
/
EOF

cat > ${script_dir}/activ.nml <<EOF
&MVCTL
  PUTMEAN = 1,'months','last',0
  meannam = 'all'
/
EOF

      #-----------------------------------------------------------------------------------------------
      #--- Create the experiment directory and copy the bin and necessary files (namelists, etc...):

      if [ ! -s ${exp_dir} ] ; then
         mkdir -p $exp_dir
      fi

      cp $model_bin ${exp_dir}/.

      cp namelist_${exp}.echam ${exp_dir}/namelist.echam
      cp namelist_${exp}.jsbach ${exp_dir}/namelist.jsbach

      for namelist_file in ${script_dir}/*.nml ; do
          cp $namelist_file ${exp_dir}/.
      done

      cp emi_spec_${exp}.txt ${exp_dir}/emi_spec.txt

      #----------------------------------------------------------------------------------------
      #--- File linking:

           #-- utility function to parse a fortran logical from a file (e.g. namelist)

           function parse_fortran_logical()
           {
       
              if [[ "$#" -ne 2 ]] ; then
                 echo "Error in function parse_fortran_logical! There should be two arguments ('parameter' and 'file')!"
                 echo "Exiting"
                 exit 1
              fi
       
              param=$1
              file=$2
       
              flag_out=`cat $file | grep -i "^ *$param *=" | tr "[:upper:]" "[:lower:]" | \
                            sed -e "s/^ *$param *=[^a-z]*\([a-z]*\)[^a-z]*/\1/"`
       
              case $flag_out in
                   true|t)
                     echo "true"
                   ;;
                   false|f)
                     echo "false"
                   ;;
              esac
       
              return
           }
       
           #-- Compute some utility variables related to run dates necessary for the file linking

           source ./get_rundates_info.sh

           #-- Set up relevant flags for input files based on user's choice in the above echam namelist

           source ./get_input_file_choices.sh

           #-- Link relevant files and check if the links are pointing to real files:

           source ./symlinks_backup_runscript_echam6-hammoz.sh

fi # end if first_launch is true

#----------------------------------------------------------------------------------------
#--- Start the run:

cd /work/bm0702/m222045/backup_runscript_echam6-hammoz/

set +e
poe ./echam6

status_echam="$?"
#
if [[ "$status_echam" -ne "0" ]] && [[ "$status_echam" -ne "127" ]] && [[ "$status_echam" -ne "1" ]] ; then
    echo "ERROR: model run stopped with return value ${status_echam}."
    exit
fi

set -e

#----------------------------------------------------------------------------------------
#--- Tar the rerun files:

ncdump="/sw/aix61/netcdf-4.1.1-rc1/bin/ncdump"
if [ -e rerun_backup_runscript_echam6-hammoz_echam ] ; then
   rerun_date=`eval $ncdump -h rerun_backup_runscript_echam6-hammoz_echam | grep vdate | cut -c12-19`
   rerun_tar=rerun_backup_runscript_echam6-hammoz_${rerun_date}.tar

   tar cvf $rerun_tar rerun_backup_runscript_echam6-hammoz_[a-z]*
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
      cd $script_dir
      declare -x first_launch=false
      llsubmit $this_script
      echo "Next job was submitted."
   fi


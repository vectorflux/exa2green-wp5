#!/client/bin/ksh
set -e
#
# This script adapts the basic run and postprocessing scripts
#  for the coupled model (MPI-ESM)
#
# information needed is the following:
# 
# USER:      Your user-id
# GROUP_ID:  Your group-id
# REPOS_NAM: Repository name
# EXP_ID:    Your (personal) experiment-id
# EXPNAME:   only predefined experiment settings (cmip5-style)
#            piControl-LR, historical-LR, rcp26/45/85-LR, 1pctCo2, piControl-P
#             piControl-MR, historical-MR (, rcp45-MR)
#            Default setting should be piControl-LR
#
#            Whenever you want to use "slight modifications" use the
#            obvious predefined experiments setting and modify the
#            scripts you generate.
#            For different namelists simply copy the lists into
#            your scripts directory and modify them. Then copy them 
#            from your scripts directory into your work.
# 
# MPIOM_EXE: Name of your mpiom-executable (see compile script output)
# ECHAM_EXE: Name of your echam-executable (see compile script output)
# OASIS_EXE: Name of your oasis-executable (see compile script output)
# EMAIL:     Your email adress
# ACCOUNT:   Your user account, usually identical to your group-id
#
# The necessary directories for the model run are designed as follows
# home: /home/zmaw/USER_ID/REPOS_NAM/experiments            for scripts etc.
# data: /work/GROUP_ID/USER_ID/REPOS_NAM/experiments   for model output
# work: /scratch/m/USER_ID/REPOS_NAM/experiments       temporary
#
#
export USER_ID=m214002
export GROUP_ID=mh0469
export REPOS_NAM=mpi-esm1-release
export EXP_ID=mbe0333
export EXPNAME=piControl-LR
export MPIOM_EXE=mpiom_hamocc_mbe0333.x
export ECHAM_EXE=echam6_mbe0333.x
export OASIS_EXE=oasis3_MPI1.x
export EMAIL=monika.esch@zmaw.de
export ACCOUNT=mh0469
#
###########################################################################
# Directories and files are generated
#
#
if [ ! -d /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments ]; then
 mkdir /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments
fi
if [ ! -d /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID} ]; then
 mkdir /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}
 mkdir /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts
fi
if [ ! -d /work/${GROUP_ID}/${USER_ID}/${REPOS_NAM} ]; then
 mkdir /work/${GROUP_ID}/${USER_ID}/${REPOS_NAM}
 mkdir /work/${GROUP_ID}/${USER_ID}/${REPOS_NAM}/experiments
fi
if [ ! -d /scratch/m/${USER_ID}/${REPOS_NAM} ]; then
 mkdir /scratch/m/${USER_ID}/${REPOS_NAM}
 mkdir /scratch/m/${USER_ID}/${REPOS_NAM}/experiments
fi
#
cp dummy.mpiesm.run /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts/dummy.run1
cp dummy.post /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts/dummy.post1
#
cd /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts
#
# generate runscript
#
case ${EXPNAME} in
  piControl-MR | historical-MR | rcp26-MR | rcp45-MR | rcp85-MR )
sed -e '1,$s/NODE/'10'/'  dummy.run1>dummy.run0
  ;;
  * )
sed -e '1,$s/NODE/'4'/'  dummy.run1>dummy.run0
  ;;
esac
sed -e '1,$s/EXP-ID/'${EXP_ID}'/' dummy.run0>dummy.run2
sed -e '1,$s/EXPNAME/'${EXPNAME}'/' dummy.run2>dummy.run1
sed -e '1,$s/USER_ID/'${USER_ID}'/' dummy.run1>dummy.run2
sed -e '1,$s/GROUP_ID/'${GROUP_ID}'/' dummy.run2>dummy.run1
sed -e '1,$s/MPIOM_EXE/'${MPIOM_EXE}'/' dummy.run1>dummy.run2
sed -e '1,$s/ECHAM_EXE/'${ECHAM_EXE}'/' dummy.run2>dummy.run1
sed -e '1,$s/OASIS_EXE/'${OASIS_EXE}'/' dummy.run1>dummy.run2
sed -e '1,$s/EMAIL/'${EMAIL}'/' dummy.run2>dummy.run1
sed -e '1,$s/ACCOUNT/'${ACCOUNT}'/' dummy.run1>dummy.run2
sed -e '1,$s/REPOS_NAM/'${REPOS_NAM}'/' dummy.run2>${EXP_ID}.run
#
rm dummy.run1 dummy.run2 dummy.run0
#
#  generate postprocessing-script
#
sed -e '1,$s/EXP-ID/'${EXP_ID}'/' dummy.post1>dummy.post2
sed -e '1,$s/EXPNAME/'${EXPNAME}'/' dummy.post2>dummy.post1
sed -e '1,$s/USER_ID/'${USER_ID}'/' dummy.post1>dummy.post2
sed -e '1,$s/GROUP_ID/'${GROUP_ID}'/' dummy.post2>dummy.post1
sed -e '1,$s/EMAIL/'${EMAIL}'/' dummy.post1>dummy.post2
sed -e '1,$s/ACCOUNT/'${ACCOUNT}'/' dummy.post2>dummy.post1
sed -e '1,$s/REPOS_NAM/'${REPOS_NAM}'/' dummy.post1>${EXP_ID}.post
#
rm dummy.post1 dummy.post2
#
#
echo 'you will find your scripts in'
echo /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts
#
#
exit

#!/client/bin/ksh
set -e
#
# This script adapts the basic run and postprocessing scripts
#  for ECHAM6
#
# information needed is the following:
# 
# USER:      Your user-id
# GROUP_ID:  Your group-id
# REPOS_NAM: Repository name
# EXP_ID:    Your (personal) experiment-id
# EXPNAME:   only predefined experiment settings (cmip5-style)
#            amip-LR, amip-MR, sstClim-LR, sstClim-MR
#
#            Default setting should be amip-LR
#
#            Whenever you want to use "slight modifications" use the
#            obvious predefined experiments setting and modify the
#            scripts you generate.
#            For different namelists simply copy the lists into
#            your scripts directory and modify them. Then copy them 
#            from your scripts directory into your work.
# 
# ECHAM_EXE: Name of your echam-executable (see compile script output)
#
# ACCOUNT:   Your user account, usually identical to your group-id
#
# The necessary directories for the model run are designed as follows
# home: /home/zmaw/USER_ID/REPOS_NAM/experiments       for scripts etc.
# data: /work/GROUP_ID/USER_ID/REPOS_NAM/experiments   for model output
#
#
export USER_ID=m214002
export GROUP_ID=mh0469
export REPOS_NAM=echam-dev
export EXP_ID=mbe0316
export EXPNAME=amip-LR
export ECHAM_EXE=echam6
export ACCOUNT=mh0469
#
###########################################################################
# Files are generated
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
#
cp dummy.echam.run /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts/dummy.run1
cp dummy.job2 /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts/dummy.post1
cp job1 /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts/job1 
#
cd /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts
#
# generate runscript and pp-script
#
case ${EXPNAME} in
  amip-MR )
   sed -e '1,$s/NODE/'4'/'  dummy.run1>dummy.run0
   sed -e '1,$s/LEVELS=47/LEVELS=95/'  dummy.post1>dummy.post0
  ;;
  * )
   sed -e '1,$s/NODE/'4'/'  dummy.run1>dummy.run0
   cp dummy.post1 dummy.post0
  ;;
esac
sed -e '1,$s/EXP-ID/'${EXP_ID}'/' dummy.run0>dummy.run2
sed -e '1,$s/EXPNAME/'${EXPNAME}'/' dummy.run2>dummy.run1
sed -e '1,$s/USER_ID/'${USER_ID}'/' dummy.run1>dummy.run2
sed -e '1,$s/GROUP_ID/'${GROUP_ID}'/' dummy.run2>dummy.run1
sed -e '1,$s/ECHAM_EXE/'${ECHAM_EXE}'/' dummy.run1>dummy.run2
sed -e '1,$s/ACCOUNT/'${ACCOUNT}'/' dummy.run2>dummy.run1
sed -e '1,$s/REPOS_NAM/'${REPOS_NAM}'/' dummy.run1>dummy.run2
cp dummy.run2 ${EXP_ID}.run_start
sed -e '1,59s/START=0/START=1/' dummy.run2>${EXP_ID}.run
#
rm dummy.run1 dummy.run2 dummy.run0
#
#  generate postprocessing-script
#
sed -e '1,$s/EXP-ID/'${EXP_ID}'/' dummy.post0>dummy.post2
sed -e '1,$s/EXPNAME/'${EXPNAME}'/' dummy.post2>dummy.post1
sed -e '1,$s/USER_ID/'${USER_ID}'/' dummy.post1>dummy.post2
sed -e '1,$s/GROUP_ID/'${GROUP_ID}'/' dummy.post2>dummy.post1
sed -e '1,$s/ACCOUNT/'${ACCOUNT}'/' dummy.post1>dummy.post2
sed -e '1,$s/REPOS_NAM/'${REPOS_NAM}'/' dummy.post2>job2
#
rm dummy.post1 dummy.post2 dummy.post0
#
#
echo 'you will find your scripts in'
echo /home/zmaw/${USER_ID}/${REPOS_NAM}/experiments/${EXP_ID}/scripts
#
#
exit

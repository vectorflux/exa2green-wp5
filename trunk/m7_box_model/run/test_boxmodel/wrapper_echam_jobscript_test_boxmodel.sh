#!/bin/bash

#------------------------------------------------------
# Script to launch a series of integrations of echam.
#------------------------------------------------------

set -e
 
dependency_option="" # initializes the batch dependency option


#---------------------------------------------------
# Step 1
#---------------------------------------------------

message=`sbatch  $dependency_option \
                 --job-name="test_boxmodel.1" \
                 --ntasks=2               \
                 --time=00:01:00              \
                 --output=slurm_test_boxmodel.1_%j.txt \
                 --error=slurm_test_boxmodel.1_%j.txt \
                 --account="s235" \
                 /users/mariaf/m7_boxmodel/grazia/run/test_boxmodel/echam_jobscript_test_boxmodel.sh `


echo "Step 1: $message"
jobid=` echo $message | awk '{print $4}'`

dependency_option="--dependency=afterok:$jobid"  # sets the dependency for next job


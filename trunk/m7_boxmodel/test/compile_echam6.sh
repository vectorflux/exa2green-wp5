#!/bin/ksh
. /sw/share/Modules/init/ksh
KEY=$1
LCOMP=$2
DIR=$3
BRANCH=$4
REVISION=$5
SVN=$6
SCR_DIR=$7
FORTRANCOMPILER=`cat ${SCR_DIR}/fortran.dat`
CCOMPILER=`cat ${SCR_DIR}/c.dat`
module load $FORTRANCOMPILER
module load $CCOMPILER
# get source of ${KEY} model and compile it 
if [ ! -d $DIR ]; then
 echo "compile_echam6.sh: directory $DIR for ${KEY} model does not exist"
 exit 1
fi
MODEL_DIR=${DIR}/${BRANCH}_rev${REVISION}
if [ -d ${MODEL_DIR} ]; then
 echo "compile_echam6.sh: found ${KEY} model ${BRANCH}_rev${REVISION}"
 cd ${MODEL_DIR}
 if [ ! -d ${BRANCH} ]; then
  echo "compile_echam6.sh: ${KEY} model directory does not contain ${BRANCH} directory"
  exit 1
 fi
 cd $BRANCH
 if [ ! -f configure ]; then
  echo "compile_echam6.sh: no configure file found in ${MODEL_DIR}/${BRANCH}"
  exit 1
 fi
 if [ $LCOMP == .false. ]; then
  if [ ! -f ${MODEL_DIR}/${BRANCH}/bin/echam6 ]; then
   LCOMP=.true.
  fi
 fi
 if [ $LCOMP == .true. ]; then
  echo "compile_echam6.sh: compile revision ${REVISION} from ${SVN}"
  make clean 1>compile.log 2>&1
  rm -f Makefile
  ./configure 1>>compile.log 2>&1
  ./util/createMakefiles.pl 2>&1
  make -j2 1>>compile.log 2>&1
  if [ -f ${MODEL_DIR}/${BRANCH}/bin/echam6 ]; then
   echo "compile_echam6.sh: compilation of ${KEY} model successful"
  else
   exit 1
  fi
 fi
else
 mkdir ${MODEL_DIR}
 cd ${MODEL_DIR}
 echo "compile_echam6.sh: checkout revision ${REVISION} from ${SVN}"
 svn checkout -r ${REVISION} ${SVN} 1>checkout.log 2>&1
 cd ${MODEL_DIR}/${BRANCH}
 echo "compile_echam6.sh: compile revision ${REVISION} from ${SVN}"
 make clean 1>compile.log 2>&1
 rm -f Makefile
 ./configure 1>>compile.log 2>&1
 ./util/createMakefiles.pl 2>&1
 make -j2 1>>compile.log 2>&1
 if [ -f ${MODEL_DIR}/${BRANCH}/bin/echam6 ]; then
  echo "compile_echam6.sh: compilation of ${KEY} model successful"
 else
  exit 1
 fi
fi

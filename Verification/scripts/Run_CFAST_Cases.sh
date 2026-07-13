#!/bin/bash
# This script runs the CFAST Verification Cases on a linux machine with
# a batch queuing system

function usage {
echo "Run_CFAST_Cases.sh [-d -h -q queue_name -s ]"
echo "Runs CFAST verification suite"
echo ""
echo "Options"
echo "-d - use debug version of cfast"
echo "-m max_iterations - stop cfast runs after a specified number of iterations (delayed stop)"
echo "     example: an option of 10 would cause cfast to stop after 10 iterations"
echo "-h - display this message"
echo "-I - compiler (intel or gnu)"
echo "-q queue_name - run cases using the queue queue_name"
echo "     default: batch"
echo "-s - stop CFAST runs"
echo "-t - output run times to a history file"
exit
}
STOPFDS=
queue=
DEBUG=
JOBPREFIX=
CURDIR=`pwd`
cd ..
export SVNROOT=`pwd`/..
smvrepo=
compiler=intel
TIME=

while getopts 'dhI:j:m:p:q:sS:t' OPTION
do
case $OPTION in
  d)
   DEBUG=_db
   ;;
  h)
  usage;
  exit
  ;;
  I)
  compiler="$OPTARG"
  ;;
  j)
  JOBPREFIX="-j $OPTARG"
  ;;
  m)
   export STOPFDSMAXITER="$OPTARG"
   ;;
  q)
   queue="$OPTARG"
   ;;
  s)
   export STOPFDS=1
   ;;
  S)
  smvrepo="$OPTARG"
  ;;
  t)
  TIME=1
  ;;
esac
#shift
done

OS=`uname`
if [ "$OS" == "Darwin" ]; then
  PLATFORM=osx
else
  PLATFORM=linux
fi
PLATFORM2=$PLATFORM
PLATFORM=$PLATFORM$DEBUG

export CFAST="$SVNROOT/Build/CFAST/${compiler}_${PLATFORM}/cfast7_$PLATFORM"

if [ "$queue" != "" ]; then
   queue="-q $queue"
fi

if [ "$TIME" == "" ]; then
  export RUNCFAST="$SVNROOT/Utilities/scripts/qcfast.sh $queue $JOBPREFIX -e $CFAST "
else
  export RUNCFAST="$SVNROOT/Validation/scripts/gettime.sh"
fi

export BASEDIR=`pwd`

echo CFAST verification cases submitted
scripts/CFAST_Cases.sh

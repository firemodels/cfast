#/bin/bash
# This script runs the FDS Verification Cases on a linux machine with
# a batch queuing system

function usage {
echo "Run_CFAST_Cases.sh [-d -h -q queue_name -s ]"
echo "Runs CFAST verification/validateion suite"
echo ""
echo "Options"
echo "-d - use debug version of cfast"
echo "-m max_iterations - stop cfast runs after a specifed number of iterations (delayed stop)"
echo "     example: an option of 10 would cause cfast to stop after 10 iterations"
echo "-h - display this message"
echo "-I - compiler (intel or gnu)"
echo "-q queue_name - run cases using the queue queue_name"
echo "     default: batch"
echo "-s - stop CFAST runs"
echo "-t - output run times to a history file"
echo "-u - use installed versions of utilities background and wind2fds"
exit
}
STOPFDS=
queue=
DEBUG=
JOBPREFIX=
use_installed=
CURDIR=`pwd`
cd ..
export SVNROOT=`pwd`/..
smvrepo=
compiler=intel
TIME=

while getopts 'dhI:j:m:p:q:sS:tu' OPTION
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
  u)
   use_installed="1"
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

if [ "$use_installed" == "1" ] ; then
  BACKGROUND=background
else
  BACKGROUND=$smvrepo/Build/background/${compiler}_${PLATFORM2}/background
fi
export BACKGROUND

export CFAST="$SVNROOT/Build/CFAST/${compiler}_${PLATFORM}/cfast7_$PLATFORM"

if [ "$queue" != "" ]; then
   queue="-q $queue"
fi

if [ "$TIME" == "" ]; then
  export RUNCFAST="$SVNROOT/Validation/scripts/qcfast.sh $queue $JOBPREFIX -V -e $CFAST "
else
  export RUNCFAST="$SVNROOT/Validation/scripts/gettime.sh"
fi

export BASEDIR=`pwd`

echo CFAST cases submitted
scripts/CFAST_Cases.sh

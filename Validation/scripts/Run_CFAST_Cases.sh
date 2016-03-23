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
echo "-p size - platform size"
echo "     default: 64"
echo "     other options: 32"
echo "-q queue_name - run cases using the queue queue_name"
echo "     default: batch"
echo "-s - stop CFAST runs"
echo "-u - use installed versions of utilities background and wind2fds"
exit
}
STOPFDS=
queue=
size=_64
DEBUG=
JOBPREFIX=
use_installed=
CURDIR=`pwd`
cd ..
export SVNROOT=`pwd`/..
fdsrepo=
compiler=intel

while getopts 'dF:hI:j:m:p:q:su' OPTION
do
case $OPTION in
  d)
   DEBUG=_db
   ;;
  h)
  usage;
  exit
  ;;
  F)
  fdsrepo="$OPTARG"
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
  p)
   size="$OPTARG"
   ;;
  q)
   queue="$OPTARG"
   ;;
  s)
   export STOPFDS=1
   ;;
  u)
   use_installed="1"
   ;;
esac
#shift
done

underscore="_"
OS=`uname`
if [ "$OS" == "Darwin" ]; then
  PLATFORM=osx$size
else
  PLATFORM=linux$size
fi
PLATFORM2=$PLATFORM
PLATFORM=$PLATFORM$DEBUG

if [ "$use_installed" == "1" ] ; then
  BACKGROUND=background
else
  BACKGROUND=$fdsrepo/Utilities/background/$compiler$underscore$PLATFORM2/background
fi
export BACKGROUND

export CFAST="$SVNROOT/Source/CFAST/$compiler$underscore$PLATFORM/cfast7_$PLATFORM"

if [ "$queue" != "" ]; then
   queue="-q $queue"
fi
export RUNCFAST="$SVNROOT/Validation/scripts/qcfast.sh $queue $JOBPREFIX -V -e $CFAST "

export BASEDIR=`pwd`

echo CFAST cases submitted
scripts/CFAST_Cases.sh

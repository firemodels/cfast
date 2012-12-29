#/bin/bash -f
echo in Run_Cfast_cases.sh
# This script runs the FDS Verification Cases on a linux machine with
# a batch queuing system

function usage {
echo "Run_CFAST_Cases.sh [-d -h -q queue_name -s ]"
echo "Runs CFAST verification/validateion suite"
echo ""
echo "Options"
echo "-d - use debug version of FDS"
echo "-h - display this message"
echo "-p size - platform size"
echo "     default: 64"
echo "     other options: 32"
echo "-q queue_name - run cases using the queue queue_name"
echo "     default: batch"
echo "     other options: fire60s, fire70s, vis"
echo "-s - stop CFAST runs"
exit
}

STOPFDS=
queue=
size=64
DEBUG=
CURDIR=`pwd`
cd ..
export SVNROOT=`pwd`/..

while getopts 'dhp:q:s' OPTION
do
case $OPTION in
  d)
   DEBUG=_db
   ;;
  h)
  usage;
  exit
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
esac
#shift
done

underscore="_"
OS=`uname`
if [ "$OS" == "Darwin" ]; then
  PLATFORM=osx$underscore$size
else
  PLATFORM=linux$underscore$score$size
fi
PLATFORM=$PLATFORM$DEBUG

export CFAST=~/cfast/CFAST/intel_$PLATFORM/cfast6_$PLATFORM
export QEXE=~/cfast/Validation/scripts/qexe.sh

if [ "$queue" != "" ]; then
   queue="-q $queue"
fi
export RUNCFAST="$SVNROOT/Validation/scripts/runcfastq.sh $queue"
export RUNCFAST2="$SVNROOT/Validation/scripts/runcfast2q.sh $queue"

export BASEDIR=`pwd`

echo CFAST cases submitted
scripts/CFAST_Cases.sh

#!/bin/bash
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
echo "--test-UI - load and rewrite each input through CEditQt instead of running CFAST"
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
TEST_UI=

args=()
while [ $# -gt 0 ]; do
  case "$1" in
    --test-UI)
      TEST_UI=1
      shift
      ;;
    *)
      args+=("$1")
      shift
      ;;
  esac
done
set -- "${args[@]}"

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

# Capture the operating system string

OS="$(uname)"

case "$OS" in
    "Darwin")
        PLATFORM="osx"
        ;;
    Linux)
        PLATFORM="linux"
        ;;
    MINGW*|MSYS*|CYGWIN*)
        PLATFORM="win"
        ;;
    *)
        PLATFORM="unknown"
        ;;
esac
PLATFORM2=$PLATFORM

if [ "$PLATFORM2" == "osx" ]; then
  CFAST_PLATFORM=macos
else
  CFAST_PLATFORM=$PLATFORM2
fi
CFAST_PLATFORM=$CFAST_PLATFORM$DEBUG
PLATFORM=$CFAST_PLATFORM

export CFAST="$SVNROOT/Build/CFAST/${compiler}_${PLATFORM}/cfast8_$CFAST_PLATFORM"

if [ "$queue" != "" ]; then
   queue="-q $queue"
fi

if [ "$TIME" == "" ]; then
  if [ "$TEST_UI" == "" ]; then
    export RUNCFAST="$SVNROOT/Validation/scripts/qcfast.sh $queue $JOBPREFIX -V -e $CFAST "
  else
    export RUNCFAST="$SVNROOT/Validation/scripts/qcedit.sh $queue $JOBPREFIX "
  fi
else
  export RUNCFAST="$SVNROOT/Validation/scripts/gettime.sh"
fi

export BASEDIR=`pwd`

if [ "$TEST_UI" == "" ]; then
  echo CFAST cases submitted
else
  echo CEditQt UI cases submitted
fi
scripts/CFAST_Cases.sh

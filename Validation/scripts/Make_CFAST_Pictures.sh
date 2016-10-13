#!/bin/bash

# This script generates smokeview pictures from the
# FDS Verification Cases on a Linux or OS X machine

function usage {
echo "Make_FDS_Pictures.sh [-d -h -r -s size -X ]"
echo "Generates Smokeview figures from FDS verification suite"
echo ""
echo "Options"
echo "-3 - make pictures using 32 bit apps"
echo "-d - use debug version of smokeview"
echo "-h - display this message"
echo "-I - compiler (intel or gnu)"
echo "-i - use installed version of smokeview"
echo "-p path - specify path of the smokeview executable"
echo "-r - use release version of smokeview"
echo "-S host - make pictures on host"
echo "-t - use test version of smokeview"
echo "-X - do not start / stop separate X-server"
exit
}

prog=$0
progname=${prog##*/}
CURDIR=`pwd`
OS=`uname`
if [ "$OS" == "Darwin" ]; then
  PLATFORM=osx
else
  PLATFORM=linux
fi

use_installed=
SIZE=64
DEBUG=
TEST=
SMV_PATH=""
START_X=yes
SSH=
COMPILER=intel

while getopts '3dhiI:p:rS:tX' OPTION
do
case $OPTION  in
  3)
   SIZE=32
   ;;
  d)
   DEBUG=_db
   ;;
  h)
   usage;
   ;;
  i)
   use_installed="1"
   ;;
  I)
   COMPILER="$OPTARG"
   ;;
  p)
   SMV_PATH="$OPTARG"
   ;;
  r)
   TEST=
  ;;
  S)
   SSH="ssh $OPTARG"
   ;;
  t)
   TEST=_test
  ;;
  X)
   START_X=no
  ;;
esac
done
shift $(($OPTIND-1))

export SVNROOT=$fdsrepo
if [ "$SMV_PATH" == "" ]; then
  SMV_PATH=$SVNROOT/SMV/Build/smokeview/$COMPILER_${PLATFORM}_${SIZE}
fi
if [ "$use_installed" == "1" ] ; then
  export SMV=smokeview
else
  export SMV=$SMV_PATH/smokeview_$PLATFORM${TEST}_${SIZE}$DEBUG
fi

export RUNSMV=$SVNROOT/Utilities/Scripts/runsmv.sh
export SMVBINDIR="-bindir $SVNROOT/SMV/for_bundle/"
export BASEDIR=`pwd`/..

echo "erasing SCRIPT_FIGURES png files"

if [ "$START_X" == "yes" ]; then
  source $SVNROOT/Utilities/Scripts/startXserver.sh 2>/dev/null
fi
cd $cfastrepo/Validation
scripts/CFAST_Pictures.sh
if [ "$START_X" == "yes" ]; then
  source $SVNROOT/Utilities/Scripts/stopXserver.sh 2>/dev/null
fi
cd $CURDIR
echo $progname complete

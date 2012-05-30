#/bin/bash -f

# This script runs the FDS Verification Cases on a linux machine with
# a batch queuing system

CURDIR=`pwd`
cd ..
export SVNROOT=`pwd`/..

#export CFASTEXE=$SVNROOT/CFAST/intel_osx_32/cfast6_osx_32
export CFASTEXE=$SVNROOT/CFAST/intel_linux_32/cfast6_linux_32
export CFAST=$CFASTEXE
export RUNCFAST=$SVNROOT/Verification/scripts/runcfast.sh
export RUNCFAST2=$SVNROOT/Verification/scripts/runcfast2.sh

export BASEDIR=`pwd`

echo CFAST cases submitted
scripts/CFAST_Cases.sh

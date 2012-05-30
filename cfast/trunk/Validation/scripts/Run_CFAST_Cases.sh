#/bin/bash -f

# This script runs the FDS Verification Cases on a linux machine with
# a batch queuing system

CURDIR=`pwd`
cd ..
export SVNROOT=`pwd`/..

export CFAST=$SVNROOT/CFAST/intel_linux_32/cfast6_linux_32
#export CFAST=$SVNROOT/CFAST/intel_osx_32/cfast6_osx_32

# not using queue
#export RUNCFAST=$SVNROOT/Validation/scripts/runcfast.sh
#export RUNCFAST2=$SVNROOT/Validation/scripts/runcfast2.sh

# using queue
export QEXE=/usr/local/bin/qexe.sh
export RUNCFAST=$SVNROOT/Validation/scripts/runcfastq.sh
export RUNCFAST2=$SVNROOT/Validation/scripts/runcfast2q.sh


export BASEDIR=`pwd`

echo CFAST cases submitted
scripts/CFAST_Cases.sh

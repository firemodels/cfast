#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR/../..
SVNROOT=`pwd`
cd $SVNROOT/Build/CFAST/intel_osx_64_db
rm *.o *.mod *.f90
./make_cfast.sh

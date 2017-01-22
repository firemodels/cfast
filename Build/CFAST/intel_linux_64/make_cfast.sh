#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

source $IFORT_COMPILER/bin/compilervars.sh $platform

echo Building $target
make -f ../makefile $target
../../../Utilities/scripts/md5hash.sh cfast7_linux_64


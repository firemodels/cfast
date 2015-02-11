#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}
echo target=$target
exit
source $IFORT_COMPILER/bin/compilervars.sh $platform

echo Building $target
make VPATH="../Source:../Include" INCLUDE="../Include" -f ../makefile $target

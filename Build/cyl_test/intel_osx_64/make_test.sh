#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

source $IFORT_COMPILER/bin/compilervars.sh $platform

make -f ../makefile intel_osx_64

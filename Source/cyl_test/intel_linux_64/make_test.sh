#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

source $IFORT_COMPILER/bin/compilervars.sh $platform

make VPATH="../source:../../CFAST/Source" -f ../makefile intel_linux_64

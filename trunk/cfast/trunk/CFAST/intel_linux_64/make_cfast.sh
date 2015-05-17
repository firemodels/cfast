#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

source $IFORT_COMPILER/bin/compilervars.sh $platform

source ../scripts/expand_file.sh ../scripts ../Source srev.f90

echo Building $target
make VPATH="../Source:../Include" INCLUDE="../Include" -f ../makefile $target

source ../scripts/contract_file.sh ../scripts ../Source/srev.f90

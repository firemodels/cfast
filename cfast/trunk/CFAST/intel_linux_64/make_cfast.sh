#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}
KWDIR=../../Utilities/keyword
SDIR=../Source


source $IFORT_COMPILER/bin/compilervars.sh $platform

echo Building $target
source $KWDIR/expand_file.sh $KWDIR $SDIR $SDIR/srev.f90
make VPATH="../Source:../Include" INCLUDE="../Include" -f ../makefile $target
source $KWDIR/contract_file.sh $KWDIR $SDIR/srev.f90


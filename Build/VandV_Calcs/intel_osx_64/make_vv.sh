#!/bin/bash
dir=`pwd`
target=${dir##*/}

source $IFORT_COMPILER/bin/compilervars.sh intel64

echo Building $target
rm -f *.o
make -f ../makefile $target

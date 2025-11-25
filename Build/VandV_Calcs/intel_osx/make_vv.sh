#!/bin/bash
dir=`pwd`
target=${dir##*/}

echo Building $target
rm -f *.o
make -f ../makefile $target

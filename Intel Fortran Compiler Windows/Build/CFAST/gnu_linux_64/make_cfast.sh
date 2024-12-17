#!/bin/bash
dir=`pwd`
target=${dir##*/}

echo Building $target
make -f ../makefile $target

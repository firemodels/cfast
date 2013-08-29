#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

echo Building $target
make VPATH="../Source:../Include" -f ../makefile $target

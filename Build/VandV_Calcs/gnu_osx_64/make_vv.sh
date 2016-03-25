#!/bin/bash
platform=gnu_osx_64
dir=`pwd`
target=${dir##*/}

echo Building $target
make -f ../makefile $target

#!/bin/bash
platform=gnu_osx
dir=`pwd`
target=${dir##*/}

echo Building $target
make -f ../makefile $target

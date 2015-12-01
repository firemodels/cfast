#!/bin/bash
platform=gnu_linux
dir=`pwd`
target=${dir##*/}

echo Building $target
make VPATH=".." -f ../makefile $target

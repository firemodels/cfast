#!/bin/bash
platform=gnu_linux_64
dir=`pwd`
target=${dir##*/}

echo Building $target
make -f ../makefile $target

#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

git clean -dxf
echo Building $target
make -f ../makefile $target

#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

make -f ../makefile intel_linux

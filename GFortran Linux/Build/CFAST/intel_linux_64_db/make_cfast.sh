#!/bin/bash
platform=intel64
dir=`pwd`
target=${dir##*/}

echo Building $target
make -f ../makefile $target
../../../Utilities/scripts/md5hash.sh cfast7_linux_64_db

#!/bin/bash
dir=`pwd`
target=${dir##*/}

echo Building $target
rm -f *.o
make -f ../makefile $target
../../../Utilities/scripts/md5hash.sh  cfast8_macos_db

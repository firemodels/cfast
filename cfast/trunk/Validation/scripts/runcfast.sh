#!/bin/bash -f
dir=$1
infile=$2

fulldir=$BASEDIR/$dir

if ! [ -e $CFAST ];  then
  echo "The file $CFAST does not exit. Run aborted"
  exit
fi
if ! [ -d $fulldir ]; then
  echo "The directory $fulldir does not exit. Run aborted."
  exit
fi
if ! [ -e $fulldir/$infile.in ]; then
  echo "The input file, $fulldir/$infile.in, does not exit. Run aborted."
  exit
fi
cd $fulldir
$CFAST $infile 

#!/bin/bash -f
dir=$1
dir2=$2
infile=$3

fulldir=$BASEDIR/$dir/$dir2

echo -----------------------------
echo running $infile in $fulldir
$QEXE $fulldir $CFAST $infile 
echo -----------------------------

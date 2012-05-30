#!/bin/bash -f
dir=$1
infile=$2

fulldir=$BASEDIR/$dir

echo -----------------------------
echo running $infile in $fulldir
$QEXE $fulldir $CFAST $infile 
echo -----------------------------

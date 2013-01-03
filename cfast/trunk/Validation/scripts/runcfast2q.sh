#!/bin/bash -f

queue=batch
while getopts 'q:' OPTION
do
case $OPTION in
  q)
   queue="$OPTARG"
   ;;
esac
#shift
done
shift $(($OPTIND-1))

dir=$1
dir2=$2
infile=$3

fulldir=$BASEDIR/$dir/$dir2

echo --------------------------------------------------
echo running $infile in $fulldir using the $queue queue
$QEXE -q $queue $dir/$dir2 $CFAST $infile /V
echo --------------------------------------------------

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
infile=$2

fulldir=$BASEDIR/$dir

echo --------------------------------------------
echo running $infile in $fulldir using the $queue queue
$QEXE -q $queue $dir $CFAST $infile 
echo --------------------------------------------

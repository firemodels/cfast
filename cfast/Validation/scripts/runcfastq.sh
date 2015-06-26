#!/bin/bash

queue=batch
while getopts 'd:q:' OPTION
do
case $OPTION in
  d)
   dir="$OPTARG"
   ;;
  q)
   queue="$OPTARG"
   ;;
esac
#shift
done
shift $(($OPTIND-1))

in=$1
infile=${in%.*}

fulldir=$BASEDIR/$dir
stopfile=$infile.stop

if [ $STOPFDS ]; then
 echo "stopping case: $infile"
 touch $fulldir/$stopfile
 exit
fi


echo --------------------------------------------
echo running $infile in $fulldir using the $queue queue
$QEXE -q $queue $dir $CFAST $infile /V
echo --------------------------------------------

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

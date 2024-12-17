#!/bin/bash

dir=.
while getopts 'd:' OPTION
do
case $OPTION in
  d)
   dir="$OPTARG"
   ;;
esac
done
shift $(($OPTIND-1))

infile=$1
basefile=${infile%.*}
logfile=$basefile.log



fulldir=$BASEDIR/$dir

if ! [ -d $fulldir ]; then
  echo "The directory $fulldir does not exist. Run aborted."
  exit
fi
if ! [ -e $fulldir/$logfile ]; then
  echo "The log file, $fulldir/$logfile, does not exit. Run aborted."
  exit
fi

time=`grep 'execution time' $fulldir/$logfile | awk '{print $5}'`
steps=`grep 'time steps' $fulldir/$logfile | awk '{print $5}'`
echo "$infile,$time,$steps"

cd $fulldir

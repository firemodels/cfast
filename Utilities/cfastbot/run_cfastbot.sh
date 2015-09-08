#!/bin/bash

running=bot_running

botscript=cfastbot_linux.sh
RUNAUTO=
while getopts 'a' OPTION
do
case $OPTION  in
  a)
   RUNAUTO=-a
   ;;
esac
done
shift $(($OPTIND-1))

if [ -e bot_running ] ; then
  echo cfasbot is already running.
  echo Erase the file $running if this is not the case.
  exit
fi

touch $running
./$botscript $RUNAUTO "$@"
rm $running

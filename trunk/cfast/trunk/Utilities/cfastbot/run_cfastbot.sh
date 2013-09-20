#!/bin/bash  

MAKEMOVIES=
LABEL=
SKIP=
QUEUE="-q fire7080s"
while getopts 'l:mq:s' OPTION
do case $OPTION in
   l)
    LABEL="-l $OPTARG"
    ;;
  m)
   MAKEMOVIES="-m"
  ;;
  q)
   QUEUE="-q $OPTARG"
  ;;
  s)
   SKIP="-s"
  ;;
esac
done
run-one bash -lc "./cfastbot_linux.sh $LABEL $SKIP $QUEUE " &

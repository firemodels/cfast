#!/bin/bash  

MAKEMOVIES=
LABEL=
SKIP=
while getopts 'l:ms' OPTION
do case $OPTION in
   l)
    LABEL="$OPTARG"
    ;;
  m)
   MAKEMOVIES="-m"
  ;;
  m)
   SKIP="-s"
  ;;
esac
done
if [ "$LABEL" != "" ]; then
  LABEL="-l $LABEL"
fi
run-one bash -lc "./cfastbot_linux.sh $LABEL $SKIP -q fire7080s " &

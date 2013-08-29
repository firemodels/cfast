#!/bin/bash  

MAKEMOVIES=
while getopts 'm' OPTION
do case $OPTION in
  m)
   MAKEMOVIES="-m"
  ;;
esac
done
run-one bash -lc "./cfastbot_linux.sh -q fire70s " &

#!/bin/bash

CFASTBOT_HOME_DIR=~
CFASTBOT_DIR="$CFASTBOT_HOME_DIR/CFASTBOT"
CFAST_SVNROOT="$CFASTBOT_HOME_DIR/cfast"
STARTED=$CFASTBOT_DIR/output/started

#for f in r30 r35 r40 r45 r50 r55 r60 r65 r70 
for f in r30 r35 
do
   while [ -e $STARTED ]; do
      echo trying to run case $f
      echo cfastbot already running
      sleep 30
   done
   echo cfastbot finished
   echo now running case $f
   cp ~/radtest/$f $CFAST_SVNROOT/CFAST/Source/radiation.f90

   cd $CFASTBOT_DIR
   ./run_cfastbot.sh -s -l $f
   sleep 30
done


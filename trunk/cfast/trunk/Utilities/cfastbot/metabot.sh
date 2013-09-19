#!/bin/bash

CFASTBOT_HOME_DIR=~
CFASTBOT_DIR="$CFASTBOT_HOME_DIR/CFASTBOT"
CFAST_SVNROOT="$CFASTBOT_HOME_DIR/cfast"
FINISH=$CFASTBOT_DIR/output/finished

cd $CFASTBOT_DIR
for f in r30 r35 r40 r45 r50 r55 r60 r65 r70 
   while [ ! -f $FINISH ]; do
      echo file $FINISH does not exist
      sleep 30
   done
   cd $CFASTBOT_DIR
   cp $f $CFAST_SVNROOT/CFAST/Source/radiation.f90
   ./run_cfastbot.sh -s -l $f
   sleep 30
done


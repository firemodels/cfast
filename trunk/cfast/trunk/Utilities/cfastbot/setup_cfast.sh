#!/bin/bash

CURDIR=`pwd`

# create a cfastbot

if [ ! -d ~/cfastbot ] ; then
  cd 
  echo ~\cfastbot does not exist - creating
  svn co http://cfast.googlecode.com/svn/trunk/cfast/trunk/Utilities/cfastbot cfastbot
  echo ~\smokebot created.
fi

# create a clean cfast repository 

if [ ! -d ~/cfastclean ] ; then
  cd 
  echo ~\cfastclean does not exist - creating
  svn co http://cfast.googlecode.com/svn/trunk/cfast/trunk cfastclean
  echo ~\cfastclean created.
fi

# create a clean FDS repository

if [ ! -d ~/FDS-SMVclean ] ; then
  cd 
  echo ~\FDS-SMVclean does not exist - creating
  svn co http://fds-smv.googlecode.com/svn/trunk/FDS/trunk FDS-SMVclean
  echo ~\FDS-SMVclean created.
fi

cd $CURDIR

#!/bin/bash
CURDIR=`pwd`
repo=$1
repodir=~/$repo

if [ -e $repodir ] ; then
  cd $repodir
  git fetch origin >& /dev/null	
  git pull >& /dev/null
  cd $CURDIR
  echo copying files
  cp $repodir/Utilities/cfastbot/*.sh .
else
  echo ***Error: The directory $repodir does not exist.  Copy aborted.
fi

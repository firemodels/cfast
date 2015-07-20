#!/bin/bash
CURDIR=`pwd`
repo=$1
repodir=~/$repo

if [ -e $repodir ] ; then
  cd $repodir
  git fetch origin
  git pull
  cp $repodir/Utilities/cfastbot/*.sh .
else
  echo ***Error: The directory $repodir does not exist.  Copy aborted.
fi

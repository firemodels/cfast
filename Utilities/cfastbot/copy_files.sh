#!/bin/bash
CURDIR=`pwd`
repo=$1
repodir=~/repo

if [ -e $repodir ] ; then
  cp $repodir/Utilities/cfastbot/*.sh .
else
  echo ***Error: The directory $repodir does not exist.  Copy aborted.
fi
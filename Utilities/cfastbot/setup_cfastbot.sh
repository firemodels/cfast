#!/bin/bash

CURDIR=`pwd`
gitrepo=FDS-SMVgitclean
gitrepodir=~/$gitrepo
botdir=~/cfastgit

# create FDS repo

if [ ! -d $gitrepodir ] ; then
  cd 
  echo $gitrepodir does not exist - creating
  git clone git@github.com:firemodels/fds-smv.git $gitrepo
  echo $gitrepodir created.
fi

# create cfast repo

gitrepo=cfastgitclean
gitrepodir=~/$gitrepo
if [ ! -d $gitrepodir ] ; then
  cd 
  echo $gitrepodir does not exist - creating
  git clone git@github.com:firemodels/cfast.git $gitrepo
  echo $gitrepodir created.
fi

# create directory where cfastbot runs

if [ ! -d $botdir ] ; then
  cd 
  echo $botdir does not exist - creating
  mkdir $botdir
  cd %botdir%
  cp $gitrepodir/Utilities/Firebot/*.sh .
  echo $botdir created.
fi

cd $CURDIR

#!/bin/bash
svn_dir=$1

export revision=unknown
export revision_date=unknown
export build_date=unknown

CURDIR=`pwd`

# looking for svn

notfound=`svn 2>&1 | tail -1 | grep "not found" | wc -l`
if [ "$notfound" == "1" ] ; then
  exit
fi

if [ ! -e "$svn_dir" ] ; then
  echo directory $svn_dir does not exist
  exit
fi

cd $svn_dir

# is this a valid svn repository

notworking=`svn 2>&1 | grep "not a working copy" | wc -l`
if [ "$notworking" == "1" ] ; then
  echo $svn_dir is not a valid svn repository
  cd $CURDIR
  exit
fi

# get svn revision number

revision=`svn info 2>&1 | grep "Last Changed Rev:" | awk -F' ' '{print $4}'`

# get svn date

revision_date=`svn info 2>&1 | grep "Last Changed Date:" | awk -F" " '{print $4,$5}'`

# get current date time

build_date=`date "+%F %T"`

cd $CURDIR
export revision
export revision_date
export build_date

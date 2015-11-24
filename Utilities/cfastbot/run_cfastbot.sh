#!/bin/bash

running=cfastbot_running

CURDIR=`pwd`
QUEUE=smokebot
cfastrepo=~/cfastgitclean
if [ -e .cfast_git ]; then
  cd ../..
  cfastrepo=`pwd`
  cd $CURDIR
fi

fdsrepo=~/FDS-SMVgitclean
if [ "$FDSSMV" != "" ] ; then
  fdsrepo=$FDSSMV
fi

function usage {
echo "Verification and validation testing script for cfast"
echo ""
echo "Options:"
echo "-a - run automatically if cfast repo has changed"
echo "-c - clean cfast and FDS-SMV repos"
echo "-f - force cfastbot run"
echo "-h - display this message"
echo "-m email -  email_address "
echo "-q queue_name - run cases using the queue queue_name"
echo "     default: $QUEUE"
echo "-C cfastrepo - cfast repository location [default: $cfastrepo]"
echo "-F fdsrepo - FDS repository location [default: $fdsrepo]"
echo "-s - skip matlab and guide generating stages"
echo "-u - update cfast and FDS-SMV repos"
echo "-v - show options used to run cfastbot"
exit
}


botscript=cfastbot_linux.sh

RUNAUTO=
UPDATEREPO=
CLEANREPO=0
RUNCFASTBOT=1
EMAIL=
FORCE=
SKIP=

while getopts 'acC:fF:hm:q:suv' OPTION
do
case $OPTION  in
  a)
   RUNAUTO=-a
   ;;
  c)
   CLEANREPO=1
   ;;
  C)
   cfastrepo="$OPTARG"
   ;;
  f)
   FORCE=1
   ;;
  F)
   fdsrepo="$OPTARG"
   ;;
  h)
   usage;
   ;;
  m)
   EMAIL="$OPTARG"
   ;;
  q)
   QUEUE="$OPTARG"
   ;;
  q)
   SKIP="-s"
   ;;
  u)
   UPDATEREPO=1
   ;;
  v)
   RUNCFASTBOT=0
   ;;
esac
done
shift $(($OPTIND-1))

if [ -e $running ] ; then
  if [ "$FORCE" == ""] ; then
    echo cfastbot is already running.
    echo Erase the file $running if this is not the case
    echo or rerun using the -f option.
    exit
  fi
fi
if [[ "$EMAIL" != "" ]]; then
  EMAIL="-m $EMAIL"
fi
if [[ "$UPDATEREPO" == "1" ]]; then
   UPDATEREPO=-u
   cd $cfastrepo
   if [[ "$RUNCFASTEBOT" == "1" ]]; then
     git remote update
     git checkout master
     git merge origin/master
     cd Utilities/cfastbot
     CFASTBOTDIR=`pwd`
     if [[ "$CURDIR" != "$FIREBOTDIR" ]]; then
       cp $botscript $CURDIR/.
     fi
     cd $CURDIR
  fi
fi
if [[ "$CLEANREPO" == "1" ]]; then
  CLEAN=-c
fi
touch $running


touch $running
QUEUE="-q $QUEUE"
cfastrepo="-C $cfastrepo"
fdsrepo="-F $fdsrepo"
cd $CURDIR
if [ "$RUNCFASTBOT" == "1" ] ; then
  ./$botscript $UPDATEREPO $CLEAN $QUEUE $fdsrepo $cfastrepo $SKIP $EMAIL "$@"
else
  echo ./$botscript $UPDATEREPO $CLEAN $QUEUE $fdsrepo $cfastrepo $SKIP $EMAIL "$@"
fi
rm $running

#!/bin/bash

if [ ! -d ~/.cfastgit ] ; then
  mkdir ~/.cfastgit
fi
running=~/.cfastgit/cfastbot_running

CURDIR=`pwd`

# checking to see if a queing system is available
QUEUE=smokebot
notfound=`qstat -a 2>&1 | tail -1 | grep "not found" | wc -l`
if [ "$notfound" == "1" ] ; then
  QUEUE=none
fi


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
echo "-U - upload guide (only by user: cfastbot)"
echo "-v - show options used to run cfastbot"
exit
}


botscript=cfastbot.sh

RUNAUTO=
UPDATEREPO=
CLEANREPO=0
RUNCFASTBOT=1
EMAIL=
FORCE=
SKIP=
UPLOAD=

while getopts 'acC:fF:hm:q:suUv' OPTION
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
  U)
   UPLOAD=-U
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
touch $running
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
     if [[ "$CURDIR" != "$CFASTBOTDIR" ]]; then
       cp $botscript $CURDIR/.
     fi
     cd $CURDIR
  fi
fi
if [[ "$CLEANREPO" == "1" ]]; then
  CLEAN=-c
fi

QUEUE="-q $QUEUE"
cfastrepo="-C $cfastrepo"
fdsrepo="-F $fdsrepo"
cd $CURDIR
if [ "$RUNCFASTBOT" == "1" ] ; then
  ./$botscript $RUNAUTO $UPDATEREPO $CLEAN $QUEUE $fdsrepo $cfastrepo $SKIP $UPLOAD $EMAIL "$@"
else
  echo ./$botscript $RUNAUTO $UPDATEREPO $CLEAN $QUEUE $fdsrepo $cfastrepo $SKIP $UPLOAD $EMAIL "$@"
fi
rm $running

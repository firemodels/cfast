#!/bin/bash

if [ ! -d ~/.cfastgit ] ; then
  mkdir ~/.cfastgit
fi
running=~/.cfastgit/cfastbot_running

CURDIR=`pwd`

# checking to see if a queing system is available
QUEUE=smokebot
notfound=`qstat -a 2>&1 | tail -1 | grep "not found" | wc -l`
if [ $notfound -eq 1 ] ; then
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
echo "-i - use installed smokeview and background (if using the 'none' queue)"
echo "-I - compiler [ default: $compiler]"
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

RUNAUTO=
UPDATEREPO=
CLEANREPO=0
RUNCFASTBOT=1
EMAIL=
FORCE=
compiler=intel

MATLABEXE=
SKIP=
havematlab=`which matlab 2> /dev/null | wc -l`

UPLOAD=
USEINSTALL=

while getopts 'acC:fF:hiI:m:q:suUv' OPTION
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
  i)
   USEINSTALL="-i"
   ;;
  I)
   compiler="$OPTARG"
   ;;
  m)
   EMAIL="$OPTARG"
   ;;
  M)
   MATLABEXE="-M"
   ;;
  q)
   QUEUE="$OPTARG"
   ;;
  s)
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
  if [ "$FORCE" == "" ]; then
    echo cfastbot is already running.
    echo Erase the file $running.  If this is
    echo not the case rerun using the -f option.
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
   if [ "$RUNCFASTBOT" == "1" ]; then
     git remote update
     git checkout master
     git merge origin/master
     cd Utilities/cfastbot
     CFASTBOTDIR=`pwd`
     if [[ "$CURDIR" != "$CFASTBOTDIR" ]]; then
       cp cfastbot.sh $CURDIR/.
     fi
     cd $CURDIR
  fi
fi
if [[ "$CLEANREPO" == "1" ]]; then
  CLEAN=-c
fi

if [ $havematlab -eq 0 ]; then
   MATLABEXE=-M
fi
if [ "$SKIP" != "" ]; then
   MATLAB=
fi

QUEUE="-q $QUEUE"
compiler="-I $compiler"
cfastrepo="-C $cfastrepo"
fdsrepo="-F $fdsrepo"
cd $CURDIR
if [ "$RUNCFASTBOT" == "1" ] ; then
  ./cfastbot.sh $USEINSTALL $RUNAUTO $compiler $UPDATEREPO $CLEAN $QUEUE $fdsrepo $cfastrepo $SKIP $MATLABEXE $UPLOAD $EMAIL "$@"
else
  echo ./$botscript $USEINSTALL $RUNAUTO $compiler $UPDATEREPO $CLEAN $QUEUE $fdsrepo $cfastrepo $SKIP $MATLABEXE $UPLOAD $EMAIL "$@"
fi
rm $running

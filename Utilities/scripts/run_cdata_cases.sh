#!/bin/bash
QCFAST="$FIREMODELS/cfast/Utilities/scripts/qcfast.sh"
CFAST=$FIREMODELS/cfast/Build/CFAST/intel_linux_64/cfast7_linux_64
GROUP=group
NCASES_PER_SCRIPT=30
NCASES=
ECHO=echo
BASE=`ls -l *-1.in | awk '{print $NF}' | awk -F'-' '{print $1}'`
if [ "$BASE" != "" ]; then
  BASE=$BASE"-"
fi

SCRIPTDIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

#---------------------------------------------
#                   usage
#---------------------------------------------

function usage {
echo "Run cfast CDATA cases in groups"
echo ""
echo "Options:"
if [ "$BASE" != "" ]; then
  echo "-b base - base name of cfast input files [default: $BASE}"
else
  echo "-b base - base name of cfast input files"
fi
echo "-e exe - location of cfast executable [default: $CFAST]"
echo "-g group - base name of scripts used to run cfast [default: $GROUP]"
echo "-h - display this message"
echo "-k - kill all jobs owned by $USER"
echo "-n num - number of cases in each group (except possibly for last group)"
echo "-N NUM - total number of cfast cases to run"
echo "-q queue - slurm queue used to run cfast jobs [default: $QUEUE]"
echo "-r      - run cases (otherwise just generate scripts)"
exit
}

while getopts 'b:e:g:hkn:N:q:r' OPTION
do
case $OPTION  in
  b)
   BASE="$OPTARG"
   ;;
  e)
   CFAST="$OPTARG"
   ;;
  g)
   GROUP="$OPTARG"
   ;;
  h)
   usage
   ;;
  k)
   echo killing all slurm jobs owned by $USER
   scancel -u $USER
   exit
   ;;
  n)
   NCASES_PER_SCRIPT="$OPTARG"
   ;;
  N)
   NCASES="$OPTARG"
   ;;
  q)
   QUEUE="$OPTARG"
   ;;
  r)
   ECHO=
   ;;
esac
done
shift $(($OPTIND-1))
error=
if [ ! -e $QCFAST ]; then
  echo "***error: the script $QCFAST for submitting jobs does not exist"
  error=1
fi
if [ ! -e $CFAST ]; then
  echo "***error: the cfast program $CFAST does not exist"
  error=1
fi
if [ "$NCASES" == "" ]; then
  echo "***error: number of cfast input files not specified"
  error=1
fi
if [ "$error" != "" ]; then
  exit
fi
if [ "$QUEUE" != "" ]; then
  QUEUE="-q $QUEUE"
fi

BEG=1
END=$((BEG + NCASES_PER_SCRIPT - 1))
if [ $END -gt $NCASES ]; then
  END=$NCASES
fi
if [ "$ECHO" != "" ]; then
  echo ""
  echo "Creating cdata group scripts."
  echo "To run cfast cases, rerun with the -r option or"
  echo "run one of the following."
  echo ""
  echo "Type scancel -u $USER to kill all jobs owned by $USER"
  echo ""
fi
i=1
while [ $END -le $NCASES ]; do
  SCRIPT=$GROUP$i
  $SCRIPTDIR/make_cdata_scripts.sh $BASE $BEG $END $SCRIPT
  if [ -e error ]; then
    echo "***some intput files exist. runs aborted"
    ECHO=echo
  fi
  $ECHO $QCFAST -S $SCRIPT $QUEUE
  BEG=`expr $BEG + $NCASES_PER_SCRIPT`
  END=`expr $END + $NCASES_PER_SCRIPT`
  if [ $BEG -gt $NCASES ]; then
    exit
  fi
  if [ $END -gt $NCASES ]; then
    END=$NCASES
  fi
  i=`expr $i + 1`
done


#!/bin/bash
QCFAST="$FIREMODELS/cfast/Utilities/scripts/qcfast.sh"
CFAST=$FIREMODELS/cfast/Build/CFAST/intel_linux_64/cfast7_linux_64
GROUP=group
NCASES_PER_SCRIPT=30
NCASES=
ECHO=echo
BASE=Tam_8_AH_A-

#---------------------------------------------
#                   usage
#---------------------------------------------

function usage {
echo "Run cfast CDATA cases"
echo ""
echo "Options:"
echo "-b base - base name of cfast input files"
echo "-e exe - location of cfast executable [default: $CFAST]"
echo "-g group - base name of scripts used to run cfast [default: $GROUP]"
echo "-h - display this message"
echo "-n num - number of cases in each script"
echo "-N NUM - number of cfast cases to run"
echo "         number of cases in each script is then NUM/num"
echo "-q queue - slurm queue used to run cfast jobs [default: $QUEUE]"
echo "-r      - run cases (otherwise just generate scripts)"
exit
}

while getopts 'b:e:g:hn:N:q:r' OPTION
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
i=1
while [ $END -le $NCASES ]; do
  SCRIPT=$GROUP$i
  ./make_scripts.sh $BASE $BEG $END $SCRIPT
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


#!/bin/bash

prog=$0

if [ $# -lt 1 ]
then
  echo "Usage: $prog [-d directory] [-q queue] [-e command] inputfile"
  echo ""
  echo "$prog runs a program specified with the -e option."
  echo ""
  echo " -b     - use debug"
  echo " -B     - location of background program"
  echo " -c     - strip extension"
  echo " -d dir - specify directory where the case is found [default: .]"
  echo " -e exe - full path of FDS used to run case"
  echo " -E     - redirect stderr to a file if the 'none' queue is used"
  echo " -j ext - specify job extension"
  echo " -q q   - name of queue. [default: batch]"
  echo "          If queue is terminal then job is run in the foreground on local computer"
  echo " -s     - stop job"
  echo " -v     - list script used to run case to standard output"
  echo " -V     - pass -V option to command being run"
  echo "input_file - input file"
  echo ""
  exit
fi

# default parameter settings

ABORTRUN=n
DB=
JOBPREFIX=

# --------------------------- parse options --------------------

# default parameter settings

queue=batch
stopjob=0

dir=.
showinput=0
strip_extension=0
erroptionfile=
VOPT=

# determine which resource manager is running (or none)

missing_slurm=`srun -V |& tail -1 | grep "not found" | wc -l`
RESOURCE_MANAGER="NONE"
if [ $missing_slurm -eq 0 ]; then
  RESOURCE_MANAGER="SLURM"
else
  missing_torque=`echo | qmgr -n |& tail -1 | grep "not found" | wc -l`
  if [ $missing_torque -eq 0 ]; then
    RESOURCE_MANAGER="TORQUE"
  fi
fi
if [ "$RESOURCE_MANAGER" == "SLURM" ]; then
  if [ "$SLURM_MEM" != "" ]; then
   SLURM_MEM="#SBATCH --mem=$SLURM_MEM"
  fi
  if [ "$SLURM_MEMPERCPU" != "" ]; then
   SLURM_MEM="#SBATCH --mem-per-cpu=$SLURM_MEMPERCPU"
  fi
fi


# read in parameters from command line

while getopts 'bB:cd:Ee:j:q:sw:vV' OPTION
do
case $OPTION  in
  b)
   DB=_db
   ;;
  B)
   BACKGROUND="$OPTARG"
   ;;
  c)
   strip_extension=1
   ;;
  d)
   dir="$OPTARG"
   ;;
  e)
   exe="$OPTARG"
   ;;
  E)
   errfileoption=1
   ;;
  j)
   JOBPREFIX="$OPTARG"
   ;;
  q)
   queue="$OPTARG"
   ;;
  s)
   stopjob=1
   ;;
  v)
   showinput=1
   ;;
  V)
   VOPT=-V
   ;;
esac
done
shift $(($OPTIND-1))

# ^^^^^^^^^^^^^^^^^^^^^^^^parse options^^^^^^^^^^^^^^^^^^^^^^^^^

#define input file

in=$1
infile=${in%.*}

# if there is more than 1 process then use the mpirun command

TITLE="$infile"

cd $dir
fulldir=`pwd`

# define files

outerr=$fulldir/$infile.err
outlog=$fulldir/$infile.log
scriptlog=$fulldir/$infile.slog
stopfile=$fulldir/$infile.stop
in_full_file=$fulldir/$in

# make sure various files exist before running case

if ! [ -e $in_full_file ]; then
  if [ "$showinput" == "0" ] ; then
    echo "The input file, $in_full_file, does not exist. Run aborted."
    ABORTRUN=y
  fi
fi
if [ "$strip_extension" == "1" ] ; then
  in=$infile
fi
if [ $STOPFDS ]; then
 echo "stopping case: $in"
 touch $stopfile
 exit
fi
if ! [ -e $exe ]; then
  if [ "$showinput" == "0" ] ; then
    echo "The program, $exe, does not exist. Run aborted."
    ABORTRUN=y
  fi
fi
if [ -e $outlog ]; then
  echo "Removing log file: $outlog"
  rm $outlog
fi
if [ "$ABORTRUN" == "y" ] ; then
  if [ "$showinput" == "0" ] ; then
    exit
  fi
fi
if [ "$STOPFDSMAXITER" != "" ]; then
  echo "creating delayed stop file: $infile"
  echo $STOPFDSMAXITER > $stopfile
fi
if [ "$stopjob" == "1" ]; then
 echo "stopping case: $in"
 touch $stopfile
 exit
fi
if [ "$STOPFDSMAXITER" == "" ]; then
  if [ -e $stopfile ]; then
    rm $stopfile
  fi
fi

QSUB="qsub -q $queue "
if [ "$RESOURCE_MANAGER" == "SLURM" ]; then
  QSUB="sbatch -p $queue --ignore-pbs"
fi

if [ "$queue" == "terminal" ] ; then
  QSUB=
fi

if [ "$queue" == "none" ]; then
  if [ "$errfileoption" == "1" ]; then
     errfileoption=" 2> $outerr"
  fi
  if [ "$BACKGROUND" == "" ]; then
    BACKGROUND=background
  fi
  notfound=`$BACKGROUND -help 2>&1 | tail -1 | grep "not found" | wc -l`
  if [ "$showinput" == "0" ]; then
    if [ $notfound -eq 1 ];  then
      echo "The program $BACKGROUND was not found."
      echo "Install FDS which has the background utility."
      echo "Run aborted"
      exit
    fi
  fi
  QSUB="$BACKGROUND -u 75 -d 1 "
fi

# create a random script file for submitting jobs
scriptfile=`mktemp /tmp/script.$$.XXXXXX`

cat << EOF > $scriptfile
#!/bin/bash
EOF

if [ "$queue" != "none" ] ; then
if [ "$RESOURCE_MANAGER" == "TORQUE" ]; then
cat << EOF >> $scriptfile
#PBS -N $JOBPREFIX$TITLE
#PBS -e $outerr
#PBS -o $outlog
#PBS -l nodes=1:ppn=1
EOF
fi
if [ "$RESOURCE_MANAGER" == "SLURM" ]; then
cat << EOF >> $scriptfile
#SBATCH -J $JOBPREFIX$TITLE
#SBATCH -e $outerr
#SBATCH -o $outlog
#SBATCH -p $queue
#SBATCH -n 1
####SBATCH --nodes=1
#SBATCH --cpus-per-task=1
EOF
fi
fi

cat << EOF >> $scriptfile
cd $fulldir
echo Running $infile on \`hostname\` 
echo       Start time: \`date\`
echo        Directory: \`pwd\`
echo Resource Manager: $RESOURCE_MANAGER
$exe $in $errfileoption $VOPT
EOF
if [ "$queue" == "none" ] ; then
cat << EOF >> $scriptfile
rm $scriptfile
EOF
fi

# if requested, output script file to screen

if [ "$showinput" == "1" ] ; then
  cat $scriptfile
  rm $scriptfile
  exit
fi

# output info to screen

if [ "$queue" != "none" ] ; then
  echo "         Input file: $in"
  echo "         Executable: $exe"
  echo "              Queue: $queue"
  echo "   Resource Manager: $RESOURCE_MANAGER"
fi

# run script

chmod +x $scriptfile
$QSUB $scriptfile
cp $scriptfile $scriptlog
if [ "$queue" != "none" ] ; then
  rm $scriptfile
fi

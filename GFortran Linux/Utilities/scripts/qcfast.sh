#!/bin/bash

# ---------------------------- stop_fds_if_requested ----------------------------------

function stop_fds_if_requested {
if [ "$OPENMPCASES" == "" ]; then
  if [ "$STOPFDS" != "" ]; then
   echo "stopping case: $in"
   touch $stopfile
   exit
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
else
  for i in `seq 1 $OPENMPCASES`; do
    stopfile=${filebase[$i]}.stop
    if [ "$STOPFDS" != "" ]; then
      echo "stopping case: ${files[$i]}"
      touch $stopfile
    fi

    if [ "$STOPFDSMAXITER" != "" ]; then
      echo "creating delayed stop file: $stopfile"
      echo $STOPFDSMAXITER > $stopfile
    fi

    if [ "$stopjob" == "1" ]; then
      echo "stopping case: ${files[$i]}"
      touch $stopfile
    fi

    if [ "$STOPFDSMAXITER" == "" ]; then
      if [ "$STOPFDS" == "" ]; then
        if [ -e $stopfile ]; then
          rm $stopfile
        fi
      fi
    fi
  done
  if [ "$STOPFDS" != "" ]; then
    exit
  fi
  if [ "$stopjob" == "1" ]; then
    exit
  fi
fi
}

# ---------------------------- usage ----------------------------------

function usage {
  if [ "$use_intel_mpi" == "1" ]; then
    MPI=impi
  else
    MPI=ompi
  fi
  echo "Usage: qfds.sh [-p n_mpi_processes] [-o nthreads] [-e fds_command] [-q queue]  casename.fds"
  echo ""
  echo "qfds.sh runs FDS using an executable from the repository or one specified with the -e option."
  echo "A parallel version of FDS is invoked by using -p to specify the number of MPI processes and/or"
  echo "-o to specify the number of OpenMP threads."
  echo ""
  echo "qfds.sh loads the modules that were loaded when fds was built unless the -C option is specified"
  echo "then the currently loaded modules are used."
  echo ""
  echo " -e exe - full path of FDS used to run case "
  echo "    [default: $FDSROOT/fds/Build/${MPI}_intel_${platform}$DB/fds_${MPI}_intel_${platform}$DB]"
  echo " -h   - show commonly used options"
  echo " -H   - show all options"
  echo " -o o - number of OpenMP threads per process [default: 1]"
  echo " -p p - number of MPI processes [default: 1] "
  echo " -q q - name of queue. [default: batch]"
  echo " -v   - output generated script to standard output"
  echo "input_file - input file"
  if [ "$HELP" == "" ]; then
    exit
  fi
  echo "Other options:"
  echo " -b email_address - send an email to email_address when jobs starts, aborts and finishes"
  echo " -d dir - specify directory where the case is found [default: .]"
  echo " -E - use tcp transport (only available with Intel compiled versions of fds)"
  echo "      This options adds export I_MPI_FABRICS=shm:tcp to the run script"
  echo " -g   - only run if input file and executable are not dirty"
  echo " -i use installed fds"
  echo " -I use Intel MPI version of fds"
  echo " -j prefix - specify a job prefix"
  echo " -L use Open MPI version of fds"
  echo " -m m - reserve m processes per node [default: 1]"
  echo " -n n - number of MPI processes per node [default: 1]"
  echo " -O n - run cases casea.fds, caseb.fds, ... using 1, ..., N OpenMP threads"
  echo "        where case is specified on the command line. N can be at most 9."
  echo " -s   - stop job"
  echo " -S   - input file is a bash script"
  echo " -t   - used for timing studies, run a job alone on a node (reserving $NCORES_COMPUTENODE cores)"
  echo " -T type - run dv (development) or db (debug) version of fds"
  echo "           if -T is not specified then the release version of fds is used"
  echo " -U n - only allow n jobs owned by `whoami` to run at a time"
  echo " -V   - show command line used to invoke qfds.sh"
  echo " -w time - walltime, where time is hh:mm for PBS and dd-hh:mm:ss for SLURM. [default: $walltime]"
  echo " -y dir - run case in directory dir"
  echo " -Y   - run case in directory casename where casename.fds is the case being run"
  echo " -z   - use --hint=nomultithread on srun line"
  echo " -Z smokezip_path - compress fds output using smokezip found at smokezip_path" 
  echo ""
  echo " Resource manager: $RESOURCE_MANAGER"
  exit
}

#*** get directory containing qfds.sh

QFDS_PATH=$(dirname `which $0`)
CURDIR=`pwd`
cd $QFDS_PATH
QFDS_DIR=`pwd`

#*** define toplevel of the repos

FDSROOT=$QFDS_DIR/../../..
cd $FDSROOT
FDSROOT=`pwd`
cd $CURDIR

SLEEP=

#*** determine platform

platform="linux"
if [ "$WINDIR" != "" ]; then
  echo "***Error: only Linux platforms are supported"
  exit
fi
if [ "`uname`" == "Darwin" ] ; then
  echo "***Error: only Linux platforms are supported"
  exit
fi

#*** determine number of cores and default queue

queue=batch
if [ "$QFDS_NCORES" == "" ]; then
  ncores=`grep processor /proc/cpuinfo | wc -l`
else
  ncores=$QFDS_NCORES
fi
if [ "$NCORES_COMPUTENODE" == "" ]; then
  NCORES_COMPUTENODE=$ncores
else
  ncores=$NCORES_COMPUTENODE
fi

#*** set default parameter values

showcommandline=
HELP=
MPIRUN=
ABORTRUN=n
DB=
OUT2ERROR=
stopjob=0
OPENMPCASES=
OPENMPTEST=

n_mpi_processes=1
n_mpi_processes_per_node=2
max_processes_per_node=`cat /proc/cpuinfo | grep cores | wc -l`
n_openmp_threads=1
use_installed=
use_debug=
use_devel=
use_intel_mpi=1
EMAIL=
CHECK_DIRTY=
casedir=
use_default_casedir=
MULTITHREAD=
USERMAX=

# use maximum number of mpi procceses possible per node
MAX_MPI_PROCESSES_PER_NODE=1

# determine which resource manager is running (or none)

STATUS_FILE=status_file.$$
srun -V >& $STATUS_FILE
missing_slurm=`cat $STATUS_FILE | tail -1 | grep "not found" | wc -l`
rm -f $STATUS_FILE

RESOURCE_MANAGER="NONE"
if [ $missing_slurm -eq 0 ]; then
  RESOURCE_MANAGER="SLURM"
else
  echo "***error: The slurm resource manager was not found and is required."
  exit
fi
if [ "$SLURM_MEM" != "" ]; then
 SLURM_MEM="#SBATCH --mem=$SLURM_MEM"
fi
if [ "$SLURM_MEMPERCPU" != "" ]; then
 SLURM_MEM="#SBATCH --mem-per-cpu=$SLURM_MEMPERCPU"
fi

# the mac doesn't have Intel MPI
if [ "`uname`" == "Darwin" ]; then
  use_intel_mpi=
fi
dir=.
benchmark=no
showinput=0
exe=
SMVZIP=
SCRIPTFILE=

if [ $# -lt 1 ]; then
  usage
fi

commandline=`echo $* | sed 's/-V//' | sed 's/-v//'`

#*** read in parameters from command line

while getopts 'Ab:d:e:EghHiIj:Lm:n:o:O:p:Pq:sS:tT:U:vVw:y:YzZ:' OPTION
do
case $OPTION  in
  A) # used by timing scripts to identify benchmark cases
   DUMMY=1
   ;;
  b)
   EMAIL="$OPTARG"
   ;;
  d)
   dir="$OPTARG"
   ;;
  e)
   exe="$OPTARG"
   ;;
  E)
   TCP=1
   ;;
  g)
   CHECK_DIRTY=1
   ;;
  h)
   usage
   exit
   ;;
  H)
   HELP=ALL
   usage
   exit
   ;;
  i)
   use_installed=1
   ;;
  I)
   use_intel_mpi=1
   ;;
  j)
   JOBPREFIX="$OPTARG"
   ;;
  L)
   use_intel_mpi=
   ;;
  m)
   max_processes_per_node="$OPTARG"
   ;;
  n)
   n_mpi_processes_per_node="$OPTARG"
   MAX_MPI_PROCESSES_PER_NODE=
   ;;
  o)
   n_openmp_threads="$OPTARG"
   ;;
  O)
   OPENMPCASES="$OPTARG"
   if [ $OPENMPCASES -gt 9 ]; then
     OPENMPCASES=9
   fi
   n_mpi_process=1
   benchmark="yes"
   ;;
  p)
   n_mpi_processes="$OPTARG"
   ;;
  P)
   OPENMPCASES="2"
   OPENMPTEST="1"
   benchmark="yes"
   n_mpi_process=1
   ;;
  q)
   queue="$OPTARG"
   ;;
  s)
   stopjob=1
   ;;
  S)
   SCRIPTFILE="$OPTARG"
   ;;
  t)
   benchmark="yes"
   ;;
  T)
   TYPE="$OPTARG"
   use_devel=
   use_debug=
   if [ "$TYPE" == "dv" ]; then
     use_devel=1
   fi
   if [ "$TYPE" == "db" ]; then
     use_debug=1
   fi
   ;;
  U)
   USERMAX="$OPTARG"
   ;;
  v)
   showinput=1
   ;;
  V)
   showcommandline=1
   ;;
  w)
   walltime="$OPTARG"
   ;;
  y)
   casedir="$OPTARG"
   ;;
  Y)
   use_default_casedir=1
   ;;
  z)
   MULTITHREAD="--hint=nomultithread"
   ;;
  Z)
   SMVZIP="$OPTARG"
   ;;
esac
done
shift $(($OPTIND-1))

if [ "$showcommandline" == "1" ]; then
  echo $0 $commandline
  exit
fi

if [ "$n_mpi_processes" == "1" ]; then
  n_mpi_processes_per_node=1
fi

# use as many processes per node as possible (fewest number of nodes)
if [ "$MAX_MPI_PROCESSES_PER_NODE" == "1" ]; then
   n_mpi_processes_per_node=$n_mpi_processes
   if test $n_mpi_processes_per_node -gt $ncores ; then
     n_mpi_processes_per_node=$ncores
   fi
fi

#*** define input file

in=$1
infile=${in%.*}
if [ "$SCRIPTFILE" != "" ]; then
  infile=$SCRIPTFILE
fi

# run case in a sub-directory
if [ "$use_default_casedir" != "" ]; then
  casedir=$infile
fi
if [ "$casedir" != "" ]; then
  if [ ! -d $casedir ]; then
    mkdir $casedir
  fi
  cp $in $casedir/.
  cd $casedir
fi

if [[ "$TCP" != "" ]] && [[ "$use_intel_mpi" == "" ]]; then
  echo "***error: The -E option for specifying tcp transport is only available"
  echo "          with Intel compiled versions of fds"
  exit
fi

if [ "$OPENMPCASES" == "" ]; then
  files[1]=$in
  filebase[1]=$infile
  nthreads[1]=$n_openmp_threads
else
  for i in `seq 1 $OPENMPCASES`; do
    nthreads[$i]=$i
    if [[ "$OPENMPTEST" == "1" ]] && [[ "$i" == "2" ]]; then
      nthreads[$i]=4
    fi
    arg=`echo ${nthreads[$i]} | tr 123456789 abcdefghi`
    filebase[$i]=$in$arg
    files[$i]=$in$arg.fds
  done
fi

#*** parse options

if [ "$walltime" == "" ]; then
  walltime=99-99:99:99
fi

#*** define executable

if [ "$use_installed" == "1" ]; then
  notfound=`echo | fds 2>&1 >/dev/null | tail -1 | grep "not found" | wc -l`
  if [ $notfound -eq 1 ]; then
    echo "fds is not installed. Run aborted."
    ABORTRUN=y
    exe=
  else
    fdspath=`which fds`
    fdsdir=$(dirname "${fdspath}")
    curdir=`pwd`
    cd $fdsdir
    exe=`pwd`/fds
    cd $curdir
  fi
else
  if [ "$use_debug" == "1" ]; then
    DB=_db
  fi
  if [ "$use_devel" == "1" ]; then
    DB=_dv
  fi
  if [ "$use_intel_mpi" == "1" ]; then
    if [ "$exe" == "" ]; then
      exe=$FDSROOT/fds/Build/impi_intel_${platform}$DB/fds_impi_intel_${platform}$DB
    fi
    if [[ $n_openmp_threads > 1 ]]; then
      exe=$FDSROOT/fds/Build/impi_intel_${platform}_openmp$DB/fds_impi_intel_${platform}_openmp$DB
    fi
  fi
  if [ "$exe" == "" ]; then
    exe=$FDSROOT/fds/Build/ompi_intel_${platform}$DB/fds_ompi_intel_${platform}$DB
  fi
fi

#*** check to see if fds was built using Intel MPI

if [ -e $exe ]; then
  if [ "$use_intel_mpi" == "" ]; then
    is_intel_mpi=`echo "" | $exe 2>&1 >/dev/null | grep MPI | grep library | grep Intel | wc -l`
    if [ "$is_intel_mpi" == "1" ]; then
         use_intel_mpi=1
    fi
  fi
fi

#*** modules loaded currently

CURRENT_LOADED_MODULES=`echo $LOADEDMODULES | tr ':' ' '`

# modules loaded when fds was built

if [ "$exe" != "" ]; then  # first look for file that contains the list
  FDSDIR=$(dirname "$exe")
  if [ -e $FDSDIR/.fdsinfo ]; then
    FDS_LOADED_MODULES=`tail -1 $FDSDIR/.fdsinfo`
    OPENMPI_PATH=`head -1 $FDSDIR/.fdsinfo`
  fi
fi

if [[ "$FDS_LOADED_MODULES" != "" ]]; then
  MODULES=$FDS_LOADED_MODULES               # modules loaded when fds was built
else
  MODULES=$CURRENT_LOADED_MODULES
fi

#*** define number of nodes

if test $n_mpi_processes_per_node -gt $ncores ; then
  n_mpi_processes_per_node=$ncores
fi

if test $n_mpi_processes_per_node = -1 ; then
  if test $n_mpi_processes -gt 1 ; then
    n_mpi_processes_per_node=2
  else
    n_mpi_processes_per_node=1
  fi
fi

let "nodes=($n_mpi_processes-1)/$n_mpi_processes_per_node+1"
if test $nodes -lt 1 ; then
  nodes=1
fi

# don't let other jobs run on nodes used by this job if you are using psm and more than 1 node
if [ "$USE_PSM" != "" ]; then
  if test $nodes -gt 1 ; then
    SLURM_PSM="#SBATCH --exclusive"
  else
    PROVIDER="export I_MPI_FABRICS=shm"
  fi
fi

#*** define processes per node

let ppn="$n_mpi_processes_per_node*n_openmp_threads"
if [ $ppn -gt $max_processes_per_node ]; then
  ppn=$max_processes_per_node
fi

if [[ $n_openmp_threads -gt 1 ]] && [[ "$use_intel_mpi" == "1" ]]; then
  ppn=2
fi

# Socket or node bindings

if test $n_mpi_processes -gt 1 ; then
 if test $n_openmp_threads -gt 1 ; then
  SOCKET_OPTION="--map-by socket:PE=$n_openmp_threads"
 else
  SOCKET_OPTION=" "
 fi
else
 SOCKET_OPTION="--map-by node:PE=$n_openmp_threads"
fi

if [ "$use_intel_mpi" == "1" ]; then
  SOCKET_OPTION=" "
fi

# Report bindings in the .err or .log file

if [ "$use_intel_mpi" == "1" ]; then
 REPORT_BINDINGS=" "
else
 REPORT_BINDINGS="--report-bindings"
fi

# The "none" queue does not use the queing system

if [ "$queue" == "none" ]; then
 SOCKET_OPTION=
 REPORT_BINDINGS=
fi

if [ "$SMVZIP" != "" ]; then
  if [ ! -e $SMVZIP ]; then
    echo "smokezip not found at $SMVZIP"
    SMVZIP=
    ABORTRUN=y
  fi
fi

#*** define MPIRUNEXE and do some error checking

if [ "$use_intel_mpi" == "1" ]; then
  if [ "$use_installed" == "1" ]; then
    MPIRUNEXE=$fdsdir/INTEL/mpi/intel64/bin/mpiexec
    if [ ! -e $MPIRUNEXE ]; then
      MPIRUNEXE=$fdsdir/INTEL/bin/mpiexec
      if [ ! -e $MPIRUNEXE ]; then
        echo "Intel mpiexec not found at:"
        echo "$fdsdir/INTEL/mpi/intel64/bin/mpiexec or"
        echo "$fdsdir/bin/mpiexec"
        echo "Run aborted"
        ABORTRUN=y
      fi
    fi
  else
    if [ "$I_MPI_ROOT" == "" ]; then
      echo "Intel MPI environment not setup. Run aborted."
      ABORTRUN=y
    else
      MPIRUNEXE=$I_MPI_ROOT/intel64/bin/mpiexec
      if [ ! -e "$MPIRUNEXE" ]; then
        MPIRUNEXE="$I_MPI_ROOT/bin/mpiexec"
        if [ ! -e "$MPIRUNEXE" ]; then
          echo "Intel mpiexec not found at:"
          echo "$I_MPI_ROOT/intel64/bin/mpiexec or"
          echo "$I_MPI_ROOT/bin/mpiexec"
          ABORTRUN=y
          echo "Run aborted"
        fi
      fi
    fi
  fi
else                                 # using OpenMPI
  if [ "$OPENMPI_PATH" != "" ]; then
    if [ -e $OPENMPI_PATH/bin ]; then
      mpibindir=$OPENMPI_PATH/bin/
    fi
  fi
  MPIRUNEXE=${mpibindir}mpirun
  if [ "$mpibindir" == "" ]; then  # OPENMPI_PATH blank so mpirun needs to be in path
    notfound=`$MPIRUNEXE -h 2>&1 >/dev/null | head -1 | grep "not found" | wc -l`
    if [ $notfound -eq 1 ]; then
      echo "*** error: $MPIRUNEXE not in PATH"
      ABORTRUN=y
    fi
  else                             # use full path to mpirun
    if [ ! -e $MPIRUNEXE ]; then
      echo "*** error: $MPIRUNEXE does not exist"
      ABORTRUN=y
    fi
  fi
fi

TITLE="$infile"
MPIRUN="$MPIRUNEXE $REPORT_BINDINGS $SOCKET_OPTION -np $n_mpi_processes"

cd $dir
fulldir=`pwd`

#*** check if exe and/or input file is dirty before running
if [[ "$CHECK_DIRTY" == "1" ]] && [[ "$exe" != "" ]]; then
  if [ -e $exe ]; then
    is_dirty_exe=`echo "" | $exe |& grep dirty |& wc -l`
    dirty_exe=`   echo "" | $exe |& grep dirty |& awk '{print $3}'`
    is_dirty_input=`git diff $in   |& wc -l`

    is_dirty=
    if [ $is_dirty_exe -gt 0 ]; then
      is_dirty=1
    fi
    if [ $is_dirty_input -gt 0 ]; then
      is_dirty=1
    fi

    if [ "$is_dirty" == "1" ]; then
      echo ""
      if [ $is_dirty_exe -gt 0 ]; then
        echo "***error: source used to build FDS is dirty."
      fi
      echo "executable: $exe"
      echo "          $dirty_exe"
      if [ $is_dirty_input -gt 0 ]; then
        echo "***error: input file $in is dirty."
      else
        echo "input file: $in"
      fi
    fi
    if [ "$is_dirty" == "1" ]; then
      echo "Use the -g option to ignore this error"
      echo "Exiting."
      exit 1
    fi
  fi
fi

#*** define files

outerr=$fulldir/$infile.err
outlog=$fulldir/$infile.log
qlog=$fulldir/$infile.qlog
stopfile=$fulldir/$infile.stop
scriptlog=$fulldir/$infile.slog
in_full_file=$fulldir/$in

#*** make sure various files exist before running the case

if [ "$OPENMPCASES" == "" ]; then
  if ! [ -e $in_full_file ]; then
    if [ "$showinput" == "0" ]; then
      echo "The input file, $in_full_file, does not exist. Run aborted."
      ABORTRUN=y
    fi
  fi
else
for i in `seq 1 $OPENMPCASES`; do
  in_full_file=$fulldir/${files[$i]}
  if ! [ -e $in_full_file ]; then
    if [ "$showinput" == "0" ]; then
      echo "The input file, $in_full_file, does not exist."
      ABORTRUN=y
    fi
  fi
done
if [ "$ABORTRUN" == "y" ]; then
  echo "Run aborted."
fi
fi

if [ "$STOPFDS" == "" ]; then
  if [ "$exe" != "" ]; then
    if ! [ -e "$exe" ]; then
      if [ "$showinput" == "0" ]; then
        echo "The program, $exe, does not exist. Run aborted."
        ABORTRUN=y
      fi
    fi
  fi

  if [ -e $outlog ]; then
    echo "Removing log file: $outlog"
    rm $outlog
  fi

  if [ "$ABORTRUN" == "y" ]; then
    if [ "$showinput" == "0" ]; then
      exit
    fi
  fi
fi

stop_fds_if_requested

#*** setup for SLURM

QSUB="sbatch -p $queue --ignore-pbs"
MPIRUN="srun -N $nodes -n $n_mpi_processes --ntasks-per-node $n_mpi_processes_per_node"

#*** Set walltime parameter only if walltime is specified as input argument

walltimestring_pbs=
walltimestring_slurm=
if [ "$walltime" != "" ]; then
  walltimestring_pbs="-l walltime=$walltime"
  walltimestring_slurm="--time=$walltime"
fi

#*** create a random script filename for submitting jobs

scriptfile=`mktemp /tmp/script.$$.XXXXXX`

cat << EOF > $scriptfile
#!/bin/bash
# $0 $commandline
EOF

cat << EOF >> $scriptfile
#SBATCH -J $JOBPREFIX$infile
#SBATCH -e $outerr
#SBATCH -o $outlog
#SBATCH --partition=$queue
#SBATCH --ntasks=$n_mpi_processes
#SBATCH --nodes=$nodes
#SBATCH --cpus-per-task=$n_openmp_threads
#SBATCH --ntasks-per-node=$n_mpi_processes_per_node
EOF
if [ "$EMAIL" != "" ]; then
    cat << EOF >> $scriptfile
#SBATCH --mail-user=$EMAIL
#SBATCH --mail-type=ALL
EOF
fi
if [ "$MULTITHREAD" != "" ]; then
    cat << EOF >> $scriptfile
#SBATCH $MULTITHREAD
EOF
fi

if [ "$benchmark" == "yes" ]; then
cat << EOF >> $scriptfile
#SBATCH --exclusive
#SBATCH --cpu-freq=Performance
EOF
fi

if [ "$SLURM_MEM" != "" ]; then
cat << EOF >> $scriptfile
$SLURM_MEM
EOF
fi

if [ "$SLURM_PSM" != "" ]; then
cat << EOF >> $scriptfile
$SLURM_PSM
EOF
fi

if [ "$walltimestring_slurm" != "" ]; then
      cat << EOF >> $scriptfile
#SBATCH $walltimestring_slurm

EOF
fi

if [[ "$MODULES" != "" ]]; then
  cat << EOF >> $scriptfile
export MODULEPATH=$MODULEPATH
module purge
module load $MODULES
EOF
fi

if [ "$OPENMPCASES" == "" ]; then
cat << EOF >> $scriptfile
export OMP_NUM_THREADS=$n_openmp_threads
EOF
fi

if [ "$use_intel_mpi" == "1" ]; then
cat << EOF >> $scriptfile
export I_MPI_DEBUG=5
EOF
fi

if [[ $n_openmp_threads -gt 1 ]] && [[ "$use_intel_mpi" == "1" ]]; then
cat << EOF >> $scriptfile
export I_MPI_PIN_DOMAIN=omp
EOF
fi

if [ "$TCP" != "" ]; then
  cat << EOF >> $scriptfile
export I_MPI_FABRICS=shm:tcp
EOF
fi

if [ "$PROVIDER" != "" ]; then
cat << EOF >> $scriptfile
$PROVIDER
EOF
fi

cat << EOF >> $scriptfile

cd $fulldir
echo
echo \`date\`
EOF

if [ "$OPENMPCASES" == "" ]; then
cat << EOF >> $scriptfile
echo "    Input file: $in"
EOF
if [ "$casedir" != "" ]; then
cat << EOF >> $scriptfile
echo "     Input dir: $casedir"
EOF
fi
else
cat << EOF >> $scriptfile
echo "    Input files: "
EOF
for i in `seq 1 $OPENMPCASES`; do
cat << EOF >> $scriptfile
echo "       ${files[$i]}"
EOF
done
fi

cat << EOF >> $scriptfile
echo "     Directory: \`pwd\`"
echo "          Host: \`hostname\`"
echo "----------------" >> $qlog
echo "started running at \`date\`" >> $qlog
EOF

if [ "$SCRIPTFILE" != "" ]; then
  exe=
fi
if [ "$OPENMPCASES" == "" ]; then
cat << EOF >> $scriptfile
bash $SCRIPTFILE $OUT2ERROR 
EOF
if [ "$SMVZIP" != "" ]; then
cat << EOF >> $scriptfile
$SMVZIP -t $n_mpi_processes_per_node $infile
EOF
fi
else
for i in `seq 1 $OPENMPCASES`; do
cat << EOF >> $scriptfile
export OMP_NUM_THREADS=${nthreads[$i]}
$MPIRUN $exe ${files[$i]} $OUT2ERROR
EOF
done
fi

cat << EOF >> $scriptfile
echo "finished running at \`date\`" >> $qlog
EOF

if [ "$queue" == "none" ]; then
cat << EOF >> $scriptfile
rm -f $scriptfile
EOF
fi

#*** output script file to screen if -v option was selected

if [ "$showinput" == "1" ]; then
  cat $scriptfile
  echo
  exit
fi

# wait until number of jobs running alread by user is less than USERMAX
if [ "$USERMAX" != "" ]; then
  nuser=`squeue | grep -v JOBID | awk '{print $4}' | grep $USER | wc -l`
  while [ $nuser -gt $USERMAX ]
  do
    nuser=`squeue | grep -v JOBID | awk '{print $4}' | grep $USER | wc -l`
    sleep 10
  done
fi

#*** output info to screen
echo "submitted at `date`"                          > $qlog
if [ "$queue" != "none" ]; then
if [ "$OPENMPCASES" == "" ]; then
  echo "         Input file:$in"             | tee -a $qlog
if [ "$casedir" != "" ]; then
  echo "          Input dir:$casedir"             | tee -a $qlog
fi
else
  echo "         Input files:"               | tee -a $qlog
for i in `seq 1 $OPENMPCASES`; do
  echo "            ${files[$i]}"            | tee -a $qlog
done
fi
  echo "         Executable:$exe"            | tee -a $qlog
  if [ "$OPENMPI_PATH" != "" ]; then
    echo "            OpenMPI:$OPENMPI_PATH" | tee -a $qlog
  fi
  if [ "$use_intel_mpi" != "" ]; then
    echo "           Intel MPI"              | tee -a $qlog
  fi

#*** output modules used when fds is run
  if [[ "$MODULES" != "" ]] && [[ "$MODULES_OUT" == "" ]]; then
    echo "            Modules:$MODULES"                    | tee -a $qlog
  fi
  echo "   Resource Manager:$RESOURCE_MANAGER"             | tee -a $qlog
  echo "              Queue:$queue"                        | tee -a $qlog
  echo "              Nodes:$nodes"                        | tee -a $qlog
  echo "          Processes:$n_mpi_processes"              | tee -a $qlog
  echo " Processes per node:$n_mpi_processes_per_node"     | tee -a $qlog
  if test $n_openmp_threads -gt 1 ; then
    echo "Threads per process:$n_openmp_threads"           | tee -a $qlog
  fi
fi

#*** run script

echo 
chmod +x $scriptfile

if [ "$queue" != "none" ]; then
  $QSUB $scriptfile | tee -a $qlog
else
  $QSUB $scriptfile
fi
if [ "$queue" != "none" ]; then
  cat $scriptfile > $scriptlog
  echo "#$QSUB $scriptfile" >> $scriptlog
  rm $scriptfile
fi

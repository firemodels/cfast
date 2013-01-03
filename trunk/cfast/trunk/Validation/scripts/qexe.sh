#!/bin/bash
# $Date: 2012-01-18 21:58:13 -0500 (Wed, 18 Jan 2012) $ 
# $Revision: 9861 $
# $Author: gforney $

PROG=$0

EXPECTED_ARGS=1

# setup default queue name

progname=qexe.sh
queue=batch

if [ $# -lt $EXPECTED_ARGS ]
then
  echo "Usage: $progname [-q queue] dir command command_arguments"
  echo ""
  echo "This script runs a program using the queing system"
  echo ""
  echo " -q queue - name of the queue. choices: [default: $queue (other choices:"  
  echo "    vis, fire60s and fire70s)"
  echo " command - full path to program to be run"
  echo " command_arguments - arguments passed to command"
  echo ""
  exit
fi

# default parameter settings

nprocesses=1
nprocesses_per_node=1
nprocesses_per_node_defined=0

# read in parameters from command line

while getopts 'q:' OPTION
do
case $OPTION  in
  q)
   queue="$OPTARG"
  ;;
esac
done
shift $(($OPTIND-1))

# set number of processes per node  to 4 if the fire60s queue is being used
# (the fire60s only have 4 cores)

dir=$1
command=$2
in=$3
option=$4
infile=${in%.*}
TITLE="$JOBPREFIX$infile(cfast)"

nnodes=1

fulldir=`pwd`/$dir

out=$fulldir/$infile.err
outlog=$fulldir/$infile.qlog
stopfile=$fulldir/$infile.stop

# make sure files that are needed exist

if ! [ -e $command ]; then
  echo "The command, $command, does not exist. Run aborted."
fi
if [ -e $outlog ]; then
  echo "Removing log file: $outlog"
  rm $outlog
fi
if [ $STOPFDS ]; then
 echo "stopping case: $in"
 touch $stopfile
 exit
fi
if [ -e $stopfile ]; then
 rm $stopfile
fi


scriptfile=/tmp/script.$$
cat << EOF > $scriptfile
#!/bin/bash -f
#PBS -N $TITLE
#PBS -e $out
#PBS -o $outlog
#PBS -l nodes=$nnodes:ppn=$nprocesses_per_node
#\$ -N $TITLE
#\$ -e $out
#\$ -o $outlog
#\$ -l nodes=$nnodes:ppn=$nprocesses_per_node

cd $fulldir
echo Start time: \`date\`
echo Running $infile on \`hostname\`
echo Directory: \`pwd\`
$command $in $option
EOF
chmod +x $scriptfile
qsub -q $queue $scriptfile
rm $scriptfile

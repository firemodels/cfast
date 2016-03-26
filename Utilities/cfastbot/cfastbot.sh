#!/bin/bash

# CFASTbot
# This script is a simplified version of Kris Overholt's Firebot script.
# It runs the CFAST verification/validation suite on the latest
# revision of the repository.

size=_64

MKDIR ()
{
  DIR=$1
  if [ ! -d $DIR ]
  then
    echo Creating directory $DIR
    mkdir $DIR
  fi
}


#  ===================
#  = Input variables =
#  ===================

mailTo="gforney@gmail.com, rpeacoc@nist.gov"

CFASTBOT_RUNDIR="`pwd`"

OUTPUT_DIR=$CFASTBOT_RUNDIR/output
HISTORY_DIR=$CFASTBOT_RUNDIR/history
ERROR_LOG=$OUTPUT_DIR/errors
TIME_LOG=$OUTPUT_DIR/timings
WARNING_LOG=$OUTPUT_DIR/warnings
NEWGUIDE_DIR=$OUTPUT_DIR/NEW_GUIDES
VALIDATION_STATS_LOG=$OUTPUT_DIR/statistics
GITSTATUS_DIR=~/.cfastbot

echo ""
echo "Preliminaries:"
echo "   running in : $CFASTBOT_RUNDIR"
MKDIR $OUTPUT_DIR
MKDIR $HISTORY_DIR
MKDIR $GITSTATUS_DIR

# define repo names (default)
export fdsrepo=~/FDS-SMVgitclean
export cfastrepo=~/cfastgitclean

COMPILER=intel
QUEUE=smokebot
RUNAUTO=
UPDATEREPO=
CLEANREPO=0
SKIP=
MATLABEXE=
UPLOAD=
USEINSTALL=
USEINSTALL2=
CCnotfound=

if [[ "$IFORT_COMPILER" != "" ]] ; then
  source $IFORT_COMPILER/bin/compilervars.sh intel64
fi

while getopts 'acC:F:hiI:m:Mq:suU' OPTION
do
case $OPTION in
   a)
     RUNAUTO="y"
     ;;
  c)
   CLEANREPO=1
   ;;
  C)
   cfastrepo="$OPTARG"
   ;;
  F)
   fdsrepo="$OPTARG"
   ;;
  h)
   usage;
   ;;
  i)
   USEINSTALL="-i"
   USEINSTALL2="-u"
   ;;
  I)
   compiler="$OPTARG"
   ;;
  m)
   mailTo="$OPTARG"
   ;;
  M)
   MATLABEXE=1
   ;;
  q)
   QUEUE="$OPTARG"
   ;;
  s)
   SKIP=1
   ;;
  u)
   UPDATEREPO=1
   ;;
  U)
   UPLOAD=1
   ;;
esac
done
shift $(($OPTIND-1))

if [ "$USEINSTALL" == "" ]; then
  CCnotfound=`icc -help 2>&1 | tail -1 | grep "not found" | wc -l`
fi

if [[ $CCnotfound -eq 0 ]] && [[ "$USEINSTALL" == "" ]]; then
  USEINSTALL=
  USEINSTALL2=
else
  USEINSTALL="-i"
  USEINSTALL2="-u"
fi

echo "   cfast repo: $cfastrepo"
echo "   FDS-SMV repo: $fdsrepo"

platform="linux"
platform2="Linux"
if [ "`uname`" == "Darwin" ] ; then
  platform="osx"
  platform2="OSX"
fi
export platform

echo "   platform: $platform2"
echo "   compiler: $compiler"

# Set unlimited stack size
if [ "$platform" == "linux" ] ; then
  ulimit -s unlimited
fi

if [ "$SKIP" == "1" ]; then
   MATLABEXE=
   echo "   matlab: skipping matlab and document building stages"
else
   if [ "$MATLABEXE" != "" ]; then
     echo "   matlab: using matlab script generated exe's"
   else
     echo "   matlab: using matlab"
   fi
fi

if [ "$UPLOAD" == "1" ]; then
  MKDIR $NEWGUIDE_DIR
fi

echo ""
echo "Status:"
echo ""

cd

THIS_CFAST_FAILED=0
CFAST_STATUS_FILE=$GITSTATUS_DIR/cfast_status
LAST_CFAST_FAILED=0
if [ -e $CFAST_STATUS_FILE ] ; then
   LAST_CFAST_FAILED=`cat $CFAST_STATUS_FILE`
fi

export JOBPREFIX=CB_
UploadGuides=$cfastrepo/Utilities/cfastbot/upload_guides.sh

#  ==============================================
#  = CFASTbot timing and notification mechanism =
#  ==============================================

# This routine checks the elapsed time of CFASTbot.
# If CFASTbot runs more than 3 hours, an email notification is sent.
# This is a notification only and does not terminate CFASTbot.
# This check runs during Stages 3 and 5.

# Start CFASTbot timer
START_TIME=$(date +%s)

# Set time limit
TIME_LIMIT=10800
TIME_LIMIT_EMAIL_NOTIFICATION="unsent"

is_changed()
{
   trigger_file=$1
   trigger_save=$2
 
   if [ ! -e $trigger_save ]; then
      touch $trigger_save
   fi
   
   THIS_REVISION=`git log --abbrev-commit $trigger_file | head -1 | awk '{print $2}'`
   LAST_REVISION=`cat $trigger_save`
   
   if [[ $THIS_REVISION == $LAST_REVISION ]] ; then
      IS_CHANGED=
   else
      IS_CHANGED=1
      echo $THIS_REVISION>$trigger_save
   fi
   
}

run_auto()
{
   CFAST_SOURCE=$cfastrepo/Source/CFAST
   git_CFASTSOURCELOG=$GITSTATUS_DIR/cfast_source_log
   MESSAGE_FILE=$GITSTATUS_DIR/message
   partial=
   
   TRIGGER=$cfastrepo/Source/CFAST
   git_TRIGGER=$GITSTATUS_DIR/cfast_source_revision
   TRIGGERONLY=$cfastrepo/Source/CFAST/skipmatlab_trigger.txt
   git_TRIGGERONLY=$GITSTATUS_DIR/cfastonly_source_revision
   
   cd $CFAST_SOURCE
   git remote update &> /dev/null
   git merge origin/master &> /dev/null

   is_changed $TRIGGER $git_TRIGGER
   if [ "$IS_CHANGED" == "" ]; then
      exit
   fi

   THIS_AUTHOR=`git log . | head -2 | tail -1 | awk '{print $2}'`

   git log . | head -5 | tail -1 > $git_CFASTSOURCELOG
  
   if [[ $THIS_REVISION == $LAST_REVISION ]] ; then
      exit
   fi

   is_changed $TRIGGERONLY $git_TRIGGERONLY
   if [ "$IS_CHANGED" == "1" ]; then
      SKIP=1
      partial=1
   fi

   rm -f $MESSAGE_FILE

   echo -e "CFAST source directory has changed. $LAST_REVISION->$THIS_CFASTSOURCE($THIS_AUTHOR)" >> $MESSAGE_FILE
   cat $git_CFASTSOURCELOG >> $MESSAGE_FILE

   echo -e "CFASTbot run initiated." >> $MESSAGE_FILE
   if [ "$partial" == "" ]; then
     cat $MESSAGE_FILE | mail -s "CFASTbot run initiated" $mailTo &> /dev/null
   else
     cat $MESSAGE_FILE | mail -s "CFASTbot run initiated (skip matlab/doc stages)" $mailTo &> /dev/null
   fi
}

check_time_limit()
{
   if [ "$TIME_LIMIT_EMAIL_NOTIFICATION" == "sent" ]
   then
      # Continue along
      :
   else
      CURRENT_TIME=$(date +%s)
      ELAPSED_TIME=$(echo "$CURRENT_TIME-$START_TIME"|bc)

      if [ $ELAPSED_TIME -gt $TIME_LIMIT ]
      then
         echo -e "CFASTbot has been running for more than 3 hours in Stage ${TIME_LIMIT_STAGE}. \n\nPlease ensure that there are no problems. \n\nThis is a notification only and does not terminate CFASTbot." | mail -s "CFASTbot Notice: CFASTbot has been running for more than 3 hours." $mailTo &> /dev/null
         TIME_LIMIT_EMAIL_NOTIFICATION="sent"
      fi
   fi
}

#  ========================
#  = Additional functions =
#  ========================

set_files_world_readable()
{
   cd $fdsrepo
   chmod -R go+r *
   cd $cfastrepo
   chmod -R go+r *
}

clean_cfastbot_history()
{
   # Clean cfastbot metafiles
   echo "Cleaning:"
   echo "   cfastbot results directory"
   cd $CFASTBOT_RUNDIR
   rm -rf $OUTPUT_DIR/* &> /dev/null
}

#  =========================
#  =========================
#  = CFASTbot Build Stages =
#  =========================
#  =========================

#  ============================
#  = Stage 1 - git operations =
#  ============================

clean_git_repo()
{
   # Check to see if FDS repository exists
   if [ -e "$fdsrepo" ]; then
      if [ "$CLEANREPO" == "1" ]; then
        echo "   FDS-SMV repo"
        echo "Cleaning FDS-SMV repo." >> $OUTPUT_DIR/stage1 2>&1
        cd $fdsrepo
        git clean -dxf &> /dev/null
        git add . &> /dev/null
        git reset --hard HEAD &> /dev/null
      fi
   else
      echo "The FDS repo $fdsrepo does not exist"
      echo "Aborting cfastbot"
      exit
   fi
   
   # Check to see if CFAST repository exists
   if [ -e "$cfastrepo" ]; then
      if [ "$CLEANREPO" == "1" ]; then
        echo "   cfast repo"
        echo "Cleaning cfast repo." >> $OUTPUT_DIR/stage1 2>&1

        cd $cfastrepo/Build
        git clean -dxf &> /dev/null
        git add . &> /dev/null
        git reset --hard HEAD &> /dev/null

        cd $cfastrepo/Verification
        git clean -dxf &> /dev/null
        git add . &> /dev/null
        git reset --hard HEAD &> /dev/null

        cd $cfastrepo/Validation
        git clean -dxf &> /dev/null
        git add . &> /dev/null
        git reset --hard HEAD &> /dev/null

        cd $cfastrepo/Manuals
        git clean -dxf &> /dev/null
        git add . &> /dev/null
        git reset --hard HEAD &> /dev/null
      fi
   else
      echo "The cfast repo $cfastrepo does not exist"
      echo "Aborting cfastbot"
      exit
   fi
}

do_git_checkout()
{
   if [ "$UPDATEREPO" == "1" ]; then
     cd $fdsrepo
     echo Checking out:
     echo "   latest FDS-SMV revision"
     echo "Checking out latest FDS-SMV revision." >> $OUTPUT_DIR/stage1 2>&1
     git remote update &> /dev/null
     git checkout development &> /dev/null
     git pull >> $OUTPUT_DIR/stage1 2>&1

     cd $cfastrepo
     echo "   latest cfast revision"
     echo "Checking out latest cfast revision." >> $OUTPUT_DIR/stage1 2>&1
     git remote update &> /dev/null
     git checkout master &> /dev/null
     git pull >> $OUTPUT_DIR/stage1 2>&1
     GIT_REVISION=`git describe --long --dirty`
   fi
}

check_git_checkout()
{
   # Check for git errors
   stage1_success=true
}

#  =================================
#  = Stage 2 - Compile CFAST       =
#  =================================

compile_cfast_db()
{
   # Build debug CFAST
   echo "Building:"
   echo "   cfast"
   echo "      debug"
   cd $cfastrepo/Build/CFAST/${compiler}_${platform}${size}_db
   make -f ../makefile clean &> /dev/null
   ./make_cfast.sh &> $OUTPUT_DIR/stage2a
 }

check_compile_cfast_db()
{
   # Check for errors in CFAST debug compilation
   cd $cfastrepo/Build/CFAST/${compiler}_${platform}${size}_db
   if [ -e "cfast7_${platform}${size}_db" ]
   then
      stage2a_success=true
   else
      echo "Errors from Stage 2a - Compile CFAST debug:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage2a >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2a` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 2a - Compile CFAST debug:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2a >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

compile_cfast()
{ 
   # Build release CFAST
   echo "      release"
   cd $cfastrepo/Build/CFAST/${compiler}_${platform}${size}
   make -f ../makefile clean &> /dev/null
   ./make_cfast.sh &> $OUTPUT_DIR/stage2b
}

check_compile_cfast()
{
   # Check for errors in CFAST release compilation
   cd $cfastrepo/Build/CFAST/${compiler}_${platform}${size}
   if [[ -e "cfast7_${platform}${size}" ]]
   then
      stage2b_success=true
   else
      echo "Errors from Stage 2b - Compile CFAST:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage2b >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2b` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 2b - Compile CFAST release:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2b >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ===================================
#  = Stage 3a - Compile SMV utilities=
#  ===================================

compile_smv_utilities()
{
   if [ "$USEINSTALL" == "" ]; then
   # smokeview libraries
     cd $fdsrepo/SMV/Build/LIBS/lib_${platform}_${compiler}${size}
     echo 'Building Smokeview libraries:' >> $OUTPUT_DIR/stage3a 2>&1
     echo "   smokeview libraries"
     ./makelibs.sh >> $OUTPUT_DIR/stage3a 2>&1

   # background
     if [ "$QUEUE" == "none" ]; then
       cd $fdsrepo/SMV/Build/background/${compiler}_${platform}${size}
       echo '   background'
       echo 'Compiling background:' >> $OUTPUT_DIR/stage3a 2>&1
       ./make_background.sh >> $OUTPUT_DIR/stage3a 2>&1
     fi
   else
     echo "   smokeview libraries - not built, using installed smokview"
     if [ "$QUEUE" == "none" ]; then
       echo "   background - not built, using installed smokeview"
       echo "Using installed smokeview, libraries not built" >> $OUTPUT_DIR/stage3a 2>&1
     fi
   fi
}

check_smv_utilities()
{
   if [ "$USEINSTALL" == "" ] ; then
     # Check for errors in SMV utilities compilation
     cd $fdsrepo
     stage3a_success="1"
     if [ "$QUEUE" == "none" ]; then
       if [ ! -e "$fdsrepo/SMV/Build/background/${compiler}_${platform}${size}/background" ]; then
         stage3a_success="0"
       fi
     fi
     if [ "$stage3asuccess" == "0" ]; then
        echo "error building background"
        echo "Errors from Stage 2c - building background:" >> $ERROR_LOG
        cat $OUTPUT_DIR/stage3a >> $ERROR_LOG
        echo "" >> $ERROR_LOG
     fi
   else
     stage3a_success="1"
     if [ "$QUEUE" == "none" ]; then
       is_file_installed background stage3a
     fi
     if [ "$stage3asuccess" == "0" ] ; then
        echo "background not installed"
        echo "Errors from Stage 2c - background not installed:" >> $ERROR_LOG
        cat $OUTPUT_DIR/stage3a >> $ERROR_LOG
        echo "" >> $ERROR_LOG
     fi
   fi
}

#  ==================================
#  = Stage 3b - Compile SMV debug   =
#  ==================================

compile_smv_db()
{
   # Clean and compile SMV DB
   if [ "$USEINSTALL" == "" ]; then
     echo "   smokeview"
     echo "      debug"
     cd $fdsrepo/SMV/Build/smokeview/${compiler}_${platform}${size}
     ./make_smv_db.sh &> $OUTPUT_DIR/stage3b
   else
     echo "   smokeview"
     echo "      debug - not built, using installed smokeview"
   fi
}

check_compile_smv_db()
{
   # Check for errors in SMV DB compilation
   if [ "$USEINSTALL" == "" ]; then
     cd $fdsrepo/SMV/Build/smokeview/${compiler}_${platform}${size}
     if [ -e "smokeview_${platform}${size}_db" ]
     then
        stage3b_success=true
     else
        echo "Errors from Stage 2d - Compile SMV DB:" >> $ERROR_LOG
        cat $OUTPUT_DIR/stage3b >> $ERROR_LOG
        echo "" >> $ERROR_LOG
     fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
     if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage3b | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
     then
        # Continue along
        :
     else
        echo "Stage 2d warnings:" >> $WARNING_LOG
        grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage3b | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
        echo "" >> $WARNING_LOG
     fi
   fi
}

#  ==================================
#  = Stage 3c - Compile SMV release =
#  ==================================

compile_smv()
{
   # Clean and compile SMV
   if [ "$USEINSTALL" == "" ]; then
     echo "      release"
     cd $fdsrepo/SMV/Build/smokeview/${compiler}_${platform}${size}
     ./make_smv.sh &> $OUTPUT_DIR/stage3c
   else
     echo "      release - not built, using installed smokeview"
   fi
}

check_compile_smv()
{
   # Check for errors in SMV release compilation
   if [ "$USEINSTALL" == "" ]; then
     cd $fdsrepo/SMV/Build/smokeview/${compiler}_${platform}${size}
     if [ -e "smokeview_${platform}${size}" ]
     then
        stage3c_success=true
     else
        echo smokeview not found
        echo "Errors from Stage 2e - Compile SMV release:" >> $ERROR_LOG
        cat $OUTPUT_DIR/stage3c >> $ERROR_LOG
        echo "" >> $ERROR_LOG
     fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
     if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage3c | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
     then
        # Continue along
        :
     else
        echo "Stage 2e warnings:" >> $WARNING_LOG
        grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage3c | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
        echo "" >> $WARNING_LOG
     fi
   else
     is_file_installed smokeview stage3c 
     if [ "$stage3c_success" == "0" ] ; then
        echo "smokeview not installed"
        echo "Errors from Stage 2e - smokeview not installed:" >> $ERROR_LOG
        cat $OUTPUT_DIR/stage1b >> $ERROR_LOG
        echo "" >> $ERROR_LOG
     fi
   fi
}

#  ========================================
#  = Stage 4 - Run V&V cases (debug mode) =
#  ========================================

wait_vv_cases_debug_start()
{
   # Scans qstat and waits for V&V cases to start
   while [[ `qstat -a | grep $(whoami) | grep -v grep | grep $JOBPREFIX | grep Q` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep -v grep | grep $JOBPREFIX | grep Q | wc -l`
      echo "Waiting for ${JOBS_REMAINING} V&V cases to start." >> $OUTPUT_DIR/stage4
      TIME_LIMIT_STAGE="4"
      check_time_limit
      sleep 30
   done
}

wait_vv_cases_debug_end()
{
   # Scans qstat and waits for V&V cases to end
   if [[ "$QUEUE" == "none" ]]
   then
     while [[ `ps -u $USER -f | fgrep .in | fgrep cfast | grep -v grep` != '' ]]; do
        JOBS_REMAINING=`ps -u $USER -f | fgrep .in | fgrep cfast | grep -v grep | wc -l`
        echo "Waiting for ${JOBS_REMAINING} verification cases to complete." >> $OUTPUT_DIR/stage4
        TIME_LIMIT_STAGE="4"
        check_time_limit
        sleep 30
     done
   else
     while [[ `qstat -a | awk '{print $2 $4}' | grep $(whoami) | grep $JOBPREFIX` != '' ]]; do
        JOBS_REMAINING=`qstat -a | awk '{print $2 $4}' | grep $(whoami) | grep $JOBPREFIX | wc -l`
        echo "Waiting for ${JOBS_REMAINING} ${1} cases to complete." >> $OUTPUT_DIR/stage4
        TIME_LIMIT_STAGE="4"
        check_time_limit
        sleep 30
     done
   fi
}

run_vv_cases_debug()
{
   cd $cfastrepo/Validation/scripts

   #  =======================
   #  = Run all cfast cases =
   #  =======================

   # Submit CFAST V&V cases
   echo 'Running CFAST V&V cases:'
   echo '   debug'
   echo 'Running CFAST V&V cases:' >> $OUTPUT_DIR/stage4 2>&1
   ./Run_CFAST_Cases.sh -I $compiler -F $fdsrepo $USEINSTALL2 -m 2 -d -j $JOBPREFIX -q $QUEUE >> $OUTPUT_DIR/stage4 2>&1
   if [ "$QUEUE" != "none" ]; then
     wait_vv_cases_debug_start
   fi

   # Wait for V&V cases to end
   wait_vv_cases_debug_end
}

check_vv_cases_debug()
{
   # Scan and report any errors in CFAST Verification cases
   cd $cfastrepo/Verification

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage4` == "" ]] && \
      [[ `grep -F "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage4 >> $OUTPUT_DIR/stage4_errors
      grep -F "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage4_errors
      grep -F "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage4_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage4_errors
      
      echo "Errors from Stage 4 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage4_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd $cfastrepo/Validation

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage4` == "" ]] && \
      [[ `grep -F "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage4 >> $OUTPUT_DIR/stage4_errors
      grep -F "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage4_errors
      grep -F "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage4_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage4_errors
      
      echo "Errors from Stage 4 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage4_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   #  =====================
   #  = Remove case files =
   #  =====================

   # Remove all unversioned case files from V&V directories (recursively)
   if [ "$CLEANREPO" == "1" ]; then
     cd $cfastrepo/Verification
     git clean -dxf &> /dev/null

     cd $cfastrepo/Validation
     git clean -dxf &> /dev/null
   fi
}

#  ==========================================
#  = Stage 5 - Run V&V cases (release mode) =
#  ==========================================

wait_vv_cases_release_start()
{
   # Scans qstat and waits for V&V cases to start
   while [[ `qstat -a | grep $(whoami) | grep -v grep | grep $JOBPREFIX | grep Q` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep -v grep | grep $JOBPREFIX | grep Q | wc -l`
      echo "Waiting for ${JOBS_REMAINING} V&V cases to start." >> $OUTPUT_DIR/stage5
      TIME_LIMIT_STAGE="5"
      check_time_limit
      sleep 30
   done
}

wait_vv_cases_release_end()
{
   # Scans qstat and waits for V&V cases to end
   if [[ "$QUEUE" == "none" ]]
   then
     while [[ `ps -u $USER -f | fgrep .in | fgrep cfast | grep -v grep` != '' ]]; do
        JOBS_REMAINING=`ps -u $USER -f | fgrep .in | fgrep cfast | grep -v grep | wc -l`
        echo "Waiting for ${JOBS_REMAINING} verification cases to complete." >> $OUTPUT_DIR/stage5
        TIME_LIMIT_STAGE="5"
        check_time_limit
        sleep 30
     done
   else
     while [[ `qstat -a | awk '{print $2 $4}' | grep $(whoami) | grep $JOBPREFIX` != '' ]]; do
        JOBS_REMAINING=`qstat -a | awk '{print $2 $4}' | grep $(whoami) | grep $JOBPREFIX | wc -l`
        echo "Waiting for ${JOBS_REMAINING} verification cases to complete." >> $OUTPUT_DIR/stage5
        TIME_LIMIT_STAGE="5"
        check_time_limit
        sleep 30
     done
   fi
}

run_vv_cases_release()
{
   # Start running all CFAST V&V cases
   cd $cfastrepo/Validation/scripts
   echo '   release'
   echo 'Running CFAST V&V cases:' >> $OUTPUT_DIR/stage5 2>&1
   ./Run_CFAST_Cases.sh -I $compiler -F $fdsrepo $USEINSTALL2 -j $JOBPREFIX -q $QUEUE >> $OUTPUT_DIR/stage5 2>&1
   if [ "$QUEUE" != "none" ]; then
     wait_vv_cases_release_start
   fi

   # Wait for all V&V cases to end
   wait_vv_cases_release_end
}

check_vv_cases_release()
{
   # Scan and report any errors in CFAST Verificaion cases
   cd $cfastrepo/Verification

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage5` == "" ]] && \
      [[ `grep -F "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage5 >> $OUTPUT_DIR/stage5_errors
      grep -F "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep -F "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      
      echo "Errors from Stage 5 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage5_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd $cfastrepo/Validation

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage5` == "" ]] && \
      [[ `grep -F "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage5 >> $OUTPUT_DIR/stage5_errors
      grep -F "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep -F "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      
      echo "Errors from Stage 5 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage5_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi
}

is_file_installed()
{
  program=$1
  stage=$2
  prognotfound=`$program -help | tail -1 | grep "not found" | wc -l`
  if [ "$prognotfound" == "1" ] ; then
    ${stage}_success="0"
    echo "***error: the $program is not installed" >> $OUTPUT_DIR/$stage
  fi
}


#  ===============================================
#  = Stage 6 - Make cfast pictures (release mode) =
#  ===============================================

make_cfast_pictures()
{
   echo "Generating smokeview images"
   cd $cfastrepo/Validation/scripts
   ./Make_CFAST_Pictures.sh -I $COMPILER $USEINSTALL 2>&1 | grep -v FreeFontPath &> $OUTPUT_DIR/stage6
}

check_cfast_pictures()
{
   # Scan and report any errors in make SMV pictures process
   cd $CFASTBOT_RUNDIR
   if [[ `grep -B 50 -A 50 "Segmentation" -I $OUTPUT_DIR/stage6` == "" && `grep -F "*** Error" -I $OUTPUT_DIR/stage6` == "" ]]
   then
      stage6_success=true
   else
      cp $OUTPUT_DIR/stage6  $OUTPUT_DIR/stage6_errors
      echo "Errors from Stage 6 - Make CFAST pictures (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage6 >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#  ====================
#  = Stage 7 - Matlab =
#  ====================

#  =============================================
#  = Stage 7a - Matlab license check           =
#  =============================================

run_matlab_license_test()
{
   echo "V&V"
   echo "   matlab license test"
   # Run simple test to see if Matlab license is available
   cd $cfastrepo/Utilities/Matlab
   matlab -logfile licmat.log -nodesktop -noFigureWindows -r "try, disp('Running Matlab License Check'), catch, disp('License Error'), err = lasterror, err.message, err.stack, end, exit" &> $OUTPUT_DIR/stage7a_matlab_license
}

scan_matlab_license_test()
{
   # Check for failed license
   if [[ `grep "License checkout failed" $OUTPUT_DIR/stage7a_matlab_license` == "" ]]
   then
      # Continue along
      :
   else
      TIME_LIMIT_STAGE="7"
      check_time_limit
      # Wait 5 minutes until retry
      sleep 300
      check_matlab_license_server
   fi
}

check_matlab_license_server()
{
   run_matlab_license_test
   scan_matlab_license_test
}

#  ==================================
#  = Stage 7b - Compile VV calcs    =
#  ==================================

compile_vvcalc()
{ 
   # Build release vvcalc
   echo "   build VandV_Calcs" 
   cd $cfastrepo/Build/VandV_Calcs/${compiler}_${platform}${size}
   make -f ../makefile clean &> /dev/null
   ./make_vv.sh &> $OUTPUT_DIR/stage7c
}

check_compile_vvcalc()
{
   cd $cfastrepo/Build/VandV_Calcs/${compiler}_${platform}${size}
   if [[ -e "VandV_Calcs_${platform}${size}" ]]
   then
      stage7c_success=true
   else
      echo "Errors from Stage 7b - Compile VandV_Calcs:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage7c >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage7c` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 7b - Compile VV calcs:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage7c >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  =============================================
#  = Stage 7c - Matlab plotting (verification) =
#  =============================================

run_matlab_verification()
{
   echo "   Verification"
   echo "      make plots"
   # Run Matlab plotting script
   cd $cfastrepo/Utilities/Matlab

   matlab -logfile vermat.log -nodesktop -noFigureWindows -r "try, disp('Running Matlab Verification script'), CFAST_verification_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $OUTPUT_DIR/stage7b_verification
}

check_matlab_verification()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_RUNDIR

   if [[ `grep -A 50 "Error" $OUTPUT_DIR/stage7b_verification` == "" ]]
   then
      stage7b_success=true
   else
      grep -A 50 "Error" $OUTPUT_DIR/stage7b_verification >> $OUTPUT_DIR/stage7b_warnings

      echo "Warnings from Stage 7c - Matlab plotting (verification):" >> $WARNING_LOG
      cat $OUTPUT_DIR/stage7b_warnings >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ==========================================================
#  = Stage 7d - Matlab plotting and statistics (validation) =
#  ==========================================================

run_matlab_validation()
{
   
   echo "   Validation"
   echo "      run VandV_Calcs"
   cd $cfastrepo/Validation
   ../Build/VandV_Calcs/${compiler}_${platform}${size}/VandV_Calcs_${platform}${size} CFAST_Pressure_Correction_inputs.csv &> /dev/null
   cp pressures.csv LLNL_Enclosure/LLNL_pressures.csv
   ../Build/VandV_Calcs/${compiler}_${platform}${size}/VandV_Calcs_${platform}${size} CFAST_Temperature_Profile_inputs.csv &> /dev/null
   cp profiles.csv Steckler_Compartment/.
   ../Build/VandV_Calcs/${compiler}_${platform}${size}/VandV_Calcs_${platform}${size} CFAST_Heat_Flux_Profile_inputs.csv &> /dev/null
   cp flux_profiles.csv Fleury_Heat_Flux/.
   
   echo "      make plots"
   # Run Matlab plotting script
   cd $cfastrepo/Utilities/Matlab
   matlab -logfile valmat.log -nodesktop -noFigureWindows -r "try, disp('Running Matlab Validation script'), CFAST_validation_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $OUTPUT_DIR/stage7d_validation
}

check_matlab_validation()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_RUNDIR
   if [[ `grep -A 50 "Error" $OUTPUT_DIR/stage7d_validation` == "" ]]
   then
      stage7d_success=true
   else
      grep -A 50 "Error" $OUTPUT_DIR/stage7d_validation >> $OUTPUT_DIR/stage7d_warnings

      echo "Warnings from Stage 7d - Matlab plotting and statistics (validation):" >> $WARNING_LOG
      cat $OUTPUT_DIR/stage7d_warnings >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

check_validation_stats()
{
   cd $cfastrepo/Utilities/Matlab

   STATS_FILE_BASENAME=CFAST_validation_scatterplot_output

   BASELINE_STATS_FILE=$cfastrepo/Utilities/Matlab/${STATS_FILE_BASENAME}_baseline.csv
   CURRENT_STATS_FILE=$cfastrepo/Utilities/Matlab/${STATS_FILE_BASENAME}.csv

   if [ -e ${CURRENT_STATS_FILE} ]
   then
      if [[ `diff -u <(sed 's/"//g' ${BASELINE_STATS_FILE}) <(sed 's/"//g' ${CURRENT_STATS_FILE})` == "" ]]
      then
         # Continue along
         :
      else
         echo "Warnings from Stage 7b - Matlab plotting and statistics (validation):" >> $VALIDATION_STATS_LOG
         echo "-------------------------------" >> $VALIDATION_STATS_LOG
         echo "Validation statistics are different from baseline statistics." >> $VALIDATION_STATS_LOG
         echo "Baseline validation statistics vs. Revision ${GIT_REVISION}:" >> $VALIDATION_STATS_LOG
         echo "-------------------------------" >> $VALIDATION_STATS_LOG
         head -n 1 ${BASELINE_STATS_FILE} >> $VALIDATION_STATS_LOG
         echo "" >> $VALIDATION_STATS_LOG
         diff -u <(sed 's/"//g' ${BASELINE_STATS_FILE}) <(sed 's/"//g' ${CURRENT_STATS_FILE}) >> $VALIDATION_STATS_LOG
         echo "" >> $VALIDATION_STATS_LOG
      fi
   else
      echo "Warnings from Stage 6c - Matlab plotting and statistics (validation):" >> $WARNING_LOG
      echo "Error: The validation statistics output file does not exist." >> $WARNING_LOG
      echo "Expected the file Utilities/Matlab/CFAST_validation_scatterplot_output.csv" >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

archive_validation_stats()
{
   cd $cfastrepo/Utilities/Matlab

   if [ -e ${CURRENT_STATS_FILE} ] ; then
      # Copy to CFASTbot history
      cp ${CURRENT_STATS_FILE} "$HISTORY_DIR/${STATS_FILE_BASENAME}_${GIT_REVISION}.csv"

      # Copy to web results
      if [ "$UPLOAD" == "1" ]; then
        cp ${CURRENT_STATS_FILE} /var/www/html/cfastbot/manuals/Validation_Statistics/${STATS_FILE_BASENAME}_${GIT_REVISION}.csv
        chmod +w /var/www/html/cfastbot/manuals/Validation_Statistics/${STATS_FILE_BASENAME}_${GIT_REVISION}.csv
      fi
   fi
}

#  ================================
#  = Stage 8 - Build CFAST Guides =
#  ================================

check_guide()
{
   # Scan and report any errors or warnings in build process for guides
   cd $CFASTBOT_RUNDIR
   if [[ `grep -I "succeeded" $1` != "" ]] && [[ -e $2/$3 ]]; then
      # Guide built succeeded; there were no errors/warnings
      # Copy guide to CFASTbot's local website
      if [ "$UPLOAD" == "1" ]; then
         cp $2/$3 /var/www/html/cfastbot/manuals/CFAST_$3
         chmod +w /var/www/html/cfastbot/manuals/CFAST_$3
      fi
   else
      # There were errors/warnings in the guide build process
      echo "Warnings from Stage 7 - Build CFAST Guides:" >> $WARNING_LOG
      echo $4 >> $WARNING_LOG # Name of guide
      if [ ! -e $2/$3 ]; then
         echo The guide $4 failed to be built >> $WARNING_LOG
         echo "" >> $WARNING_LOG
      fi 
      cat $1 >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

make_cfast_tech_guide()
{
   # Build CFAST Tech Guide
   echo Building CFAST Tech guide
   cd $cfastrepo/Manuals/Tech_Ref
   ./make_guide.sh &> $OUTPUT_DIR/stage8_cfast_tech_guide

   # Check guide for completion and copy to website if successful
   check_guide $OUTPUT_DIR/stage8_cfast_tech_guide $cfastrepo/Manuals/Tech_Ref Tech_Ref.pdf 'CFAST Technical Reference Guide'
}

make_cfast_user_guide()
{
   # Build CFAST User Guide
   echo Building CFAST User guide
   cd $cfastrepo/Manuals/Users_Guide
   ./make_guide.sh &> $OUTPUT_DIR/stage8_cfast_user_guide

   # Check guide for completion and copy to website if successful
   check_guide $OUTPUT_DIR/stage8_cfast_user_guide $cfastrepo/Manuals/Users_Guide Users_Guide.pdf 'CFAST Users Guide'
}

make_cfast_vv_guide()
{
   # Build CFAST Tech Guide
   echo Building CFAST VV guide
   cd $cfastrepo/Manuals/Validation_Guide
   ./make_guide.sh &> $OUTPUT_DIR/stage8_cfast_vv_guide

   # Check guide for completion and copy to website if successful
   check_guide $OUTPUT_DIR/stage8_cfast_vv_guide $cfastrepo/Manuals/Validation_Guide Validation_Guide.pdf 'CFAST Verification and Validation Guide'
}

make_cfast_config_guide()
{
   # Build CFAST Configuration Guide
   echo Building CFAST Configuration guide
   cd $cfastrepo/Manuals/Configuration_Guide
   ./make_guide.sh &> $OUTPUT_DIR/stage8_cfast_config_guide

   # Check guide for completion and copy to website if successful
   check_guide $OUTPUT_DIR/stage8_cfast_config_guide $cfastrepo/Manuals/Configuration_Guide Configuration_Guide.pdf 'CFAST Configuration Guide'
}

#  =====================================================
#  = Build status reporting - email and save functions =
#  =====================================================

save_build_status()
{
   cd $CFASTBOT_RUNDIR
   # Save status outcome of build to a text file
   if [[ -e $WARNING_LOG && -e $ERROR_LOG ]]
   then
     cat "" >> $ERROR_LOG
     cat $WARNING_LOG >> $ERROR_LOG
     echo "Build failure and warnings for Revision ${GIT_REVISION}." > "$HISTORY_DIR/${GIT_REVISION}.txt"
     cat $ERROR_LOG > "$HISTORY_DIR/${GIT_REVISION}_errors.txt"
     touch $OUTPUT_DIR/status_errors_and_warnings

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
      echo "Build failure for Revision ${GIT_REVISION}." > "$HISTORY_DIR/${GIT_REVISION}.txt"
      cat $ERROR_LOG > "$HISTORY_DIR/${GIT_REVISION}_errors.txt"
      touch $OUTPUT_DIR/status_errors

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
      echo "Revision ${GIT_REVISION} has warnings." > "$HISTORY_DIR/${GIT_REVISION}.txt"
      cat $WARNING_LOG > "$HISTORY_DIR/${GIT_REVISION}_warnings.txt"
      touch $OUTPUT_DIR/status_warnings

   # No errors or warnings
   else
      echo "Build success! Revision ${GIT_REVISION} passed all build tests." > "$HISTORY_DIR/${GIT_REVISION}.txt"
      touch $OUTPUT_DIR/status_success
   fi
}

email_build_status()
{
   echo $THIS_CFAST_FAILED>$CFAST_STATUS_FILE
   stop_time=`date`
   if [[ $SKIP_git_UPDATE_AND_PROPFIX ]] ; then
      echo "CFASTbot was invoked with the -s option (SKIP_git_UPDATE_AND_PROPFIX)." >> $TIME_LOG
      echo "Skipping git revert, update, and property fix operations." >> $TIME_LOG
      echo "The current git revision is ${GIT_REVISION}" >> $TIME_LOG
   fi
   echo "-------------------------------" >> $TIME_LOG
   echo "Host: $hostname " >> $TIME_LOG
   echo "Start Time: $start_time " >> $TIME_LOG
   echo "Stop Time: $stop_time " >> $TIME_LOG
   if [[ "$UPLOAD" == "1" ]]; then
      echo "-------------------------------" >> $TIME_LOG
      echo "Manuals (private): http://blaze.nist.gov/cfastbot/manuals" >> $TIME_LOG
      echo "Manuals  (public): https://goo.gl/jR6uSj" >> $TIME_LOG
      echo "-------------------------------" >> $TIME_LOG
   fi
   if [[ $THIS_REVISION != $LAST_CFASTSOUCEgit ]] ; then
     cat $git_CFASTSOURCELOG >> $TIME_LOG
   fi
   cd $CFASTBOT_RUNDIR
   # Check for warnings and errors
   if [[ -e $WARNING_LOG && -e $ERROR_LOG ]]
   then
     cat $TIME_LOG >> $ERROR_LOG
     cat $TIME_LOG >> $WARNING_LOG
     # Send email with failure message and warnings, body of email contains appropriate log file
     mail -s "CFASTbot build failure and warnings on ${hostname}. Revision ${GIT_REVISION}." $mailTo < $ERROR_LOG &> /dev/null

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
     cat $TIME_LOG >> $ERROR_LOG
      # Send email with failure message, body of email contains error log file
      mail -s "CFASTbot build failure on ${hostname}. Revision ${GIT_REVISION}." $mailTo < $ERROR_LOG &> /dev/null

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
     cat $TIME_LOG >> $WARNING_LOG
      # Send email with success message, include warnings
      mail -s "CFASTbot build success with warnings on ${hostname}. Revision ${GIT_REVISION}." $mailTo < $WARNING_LOG &> /dev/null

   # No errors or warnings
   else
      # Send empty email with success message
      mail -s "CFASTbot build success on ${hostname}! Revision ${GIT_REVISION}." $mailTo < $TIME_LOG &> /dev/null
   fi

   # Send email notification if validation statistics have changed.
   if [ -e $VALIDATION_STATS_LOG ]
   then
      mail -s "CFASTbot notice. Validation statistics have changed for Revision ${GIT_REVISION}." $mailTo < $VALIDATION_STATS_LOG &> /dev/null      
   fi
   if [[ "$UPLOADGUIDES" == "1" ]]; then
     if [ -e $UploadGuides ]; then
        $UploadGuides $NEWGUIDE_DIR $cfastrepo/Manuals > /dev/null
     fi
   fi

}

# if -a option is invoked, only proceed running CFASTbot if the smokeview or CFAST source has changed

if [[ "$RUNAUTO" == "y" ]] ; then
  run_auto
fi

#  ============================
#  = Primary script execution =
#  ============================

hostname=`hostname`
start_time=`date`

### Clean up on start ###
clean_cfastbot_history

### Stage 1 ###
clean_git_repo
do_git_checkout
check_git_checkout

### Stage 2a ###
compile_cfast_db
check_compile_cfast_db

### Stage 2b ###
compile_cfast
check_compile_cfast

### Stage 2c ###
compile_smv_utilities
check_smv_utilities

### Stage 2d ###
compile_smv_db
check_compile_smv_db

### Stage 2e ###
compile_smv
check_compile_smv

### Stage 3 ###
if [[ $stage2a_success ]] ; then
   run_vv_cases_debug
   check_vv_cases_debug
fi

### Stage 4 ###
if [[ $stage2b_success ]] ; then
   run_vv_cases_release
   check_vv_cases_release
fi

### Stage 5 ###
if [[ $stage2b_success && $stage3c_success ]] ; then
   make_cfast_pictures
   check_cfast_pictures
fi

### Stage 6a ###
if [[ "$SKIP" == "" ]]; then
  if [ "$MATLABEXE" == "" ]; then
    check_matlab_license_server
    run_matlab_verification
    check_matlab_verification
  fi
fi

### Stage 6b ###
if [[ "$SKIP" == "" ]]; then
  if [ "$MATLABEXE" == "" ]; then
    compile_vvcalc
    check_compile_vvcalc
  fi
fi

### Stage 6c ###
if [[ "$SKIP" == "" ]]; then
  if [ "$MATLABEXE" == "" ]; then
    run_matlab_validation
    check_matlab_validation
    check_validation_stats
    archive_validation_stats
  fi
fi

### Stage 7 ###
if [[ "$SKIP" == "" ]]; then
  make_cfast_tech_guide
  make_cfast_user_guide
  make_cfast_vv_guide
  make_cfast_config_guide
fi

### Report results ###
set_files_world_readable
save_build_status
email_build_status
echo cfastbot complete

#!/bin/bash

# CFASTbot
# This script is a simplified version of Kris Overholt's Firebot script.
# It runs the CFAST verification/validation suite on the latest
# revision of the repository.

#  ===================
#  = Input variables =
#  ===================

mailTo="gforney@gmail.com, rpeacoc@nist.gov"

CFASTBOT_RUNDIR="`pwd`"

OUTPUT_DIR=$CFASTBOT_RUNDIR/output
ERROR_LOG=$OUTPUT_DIR/errors
TIME_LOG=$OUTPUT_DIR/timings
WARNING_LOG=$OUTPUT_DIR/warnings
VALIDATION_STATS_LOG=$OUTPUT_DIR/statistics
GIT_STATUSDIR=~/.cfastbot

# define repo names (default)
fdsrepo=~/FDS-SMVgitclean
cfastrepo=~/cfastgitclean


CFASTBOT_QUEUE=smokebot
RUNAUTO=
UPDATEREPO=
CLEANREPO=0
SKIP=

reponame=~/cfastgitclean
while getopts 'acC:F:hm:q:su' OPTION
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
  m)
   mailTo="$OPTARG"
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
esac
done
shift $(($OPTIND-1))

platform="linux"
if [ "`uname`" == "Darwin" ] ; then
  platform="osx"
fi
export platform

# Set unlimited stack size
if [ "$platform" == "linux" ] ; then
  ulimit -s unlimited
fi

cd

THIS_CFAST_FAILED=0
CFAST_STATUS_FILE=$GIT_STATUSDIR/cfast_status
LAST_CFAST_FAILED=0
if [ -e $CFAST_STATUS_FILE ] ; then
   LAST_CFAST_FAILED=`cat $CFAST_STATUS_FILE`
fi

export JOBPREFIX=CB_

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

run_auto()
{
   SMV_SOURCE=$fdsrepo/SMV/source
   git_SMVFILE=$GIT_STATUSDIR/smokeview_source_revision
   git_SMVLOG=$GIT_STATUSDIR/smokeview_source_log

   CFAST_SOURCE=$cfastrepo/CFAST/Source
   git_CFASTSOURCEFILE=$GIT_STATUSDIR/cfast_source_revision
   git_CFASTSOURCELOG=$GIT_STATUSDIR/cfast_source_log
  
   CFAST_DOCS=$cfastrepo/Docs
   git_CFASTDOCSFILE=$GIT_STATUSDIR/cfast_docs_revision
   git_CFASTDOCSLOG=$GIT_STATUSDIR/cfast_docs_log

   SMOKEBOTDIR=~/CFASTBOT/
   SMOKEBOTEXE=./run_cfastbot.sh

   MESSAGE_FILE=$GIT_STATUSDIR/message

   cd $CFAST_SOURCE
   git pull &> /dev/null
   THIS_REVISION=`git log --abbrev-commit . | head -1 | awk '{print $2}'`
   LAST_REVISION=`cat $git_CFASTSOURCEFILE`

   
   THIS_AUTHOR=`git log . | head -2 | tail -1 | awk '{print $2}'`

   git log . | head -5 | tail -1 > $git_CFASTSOURCELOG
  
   if [[ $THIS_REVISION == $LAST_REVISION ]] ; then
      exit
   fi

   rm -f $MESSAGE_FILE

   echo $THIS_REVISION>$git_CFASTSOURCEFILE
   echo -e "CFAST source directory has changed. $LAST_REVISION->$THIS_CFASTSOURCE($THIS_AUTHOR)" >> $MESSAGE_FILE
   cat $git_CFASTSOURCELOG >> $MESSAGE_FILE

   echo -e "CFASTbot run initiated." >> $MESSAGE_FILE
   cat $MESSAGE_FILE | mail -s "CFASTbot run initiated" $mailTo > /dev/null
}

MKDIR ()
{
  DIR=$1
  if [ ! -d $DIR ]
  then
    echo Creating directory $DIR
    mkdir $DIR
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
         echo -e "CFASTbot has been running for more than 3 hours in Stage ${TIME_LIMIT_STAGE}. \n\nPlease ensure that there are no problems. \n\nThis is a notification only and does not terminate CFASTbot." | mail -s "CFASTbot Notice: CFASTbot has been running for more than 3 hours." $mailTo > /dev/null
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
   cd $CFASTBOT_RUNDIR
   MKDIR $GIT_STATUSDIR >& /dev/null
   MKDIR $OUTPUT_DIR &> /dev/null
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
        echo "Cleaning FDS-SMV repo." >> $OUTPUT_DIR/stage1 2>&1
        cd $fdsrepo
        git clean -dxf > /dev/null
        git add . > /dev/null
        git reset --hard HEAD > /dev/null
      fi
   else
      echo "The FDS repo $fdsrepo does not exist"
      echo "Aborting cfastbot"
      exit
   fi
   
   # Check to see if CFAST repository exists
   if [ -e "$cfastrepo" ]; then
      if [ "$CLEANREPO" == "1" ]; then
        echo "Cleaning cfast repo." >> $OUTPUT_DIR/stage1 2>&1
        cd $cfastrepo
        git clean -dxf > /dev/null
        git add . > /dev/null
        git reset --hard HEAD > /dev/null
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
     echo "Checking out latest FDS-SMV revision." >> $OUTPUT_DIR/stage1 2>&1
     git remote update
     git checkout development
     git pull >> $OUTPUT_DIR/stage1 2>&1

     cd $cfastrepo
     echo "Checking out latest CFAST revision." >> $OUTPUT_DIR/stage1 2>&1
     git remote update
     git checkout master
     git pull >> $OUTPUT_DIR/stage1 2>&1
     git_REVISION=`tail -n 1 $OUTPUT_DIR/stage1 | sed "s/[^0-9]//g"`
   fi
}

check_git_checkout()
{
   # Check for git errors
   stage1_success=true
}

#  =================================
#  = Stage 2 - Compile CFAST debug =
#  =================================

compile_cfast_db()
{
   # Build debug CFAST
   cd $cfastrepo/CFAST/intel_linux_64_db
   make -f ../makefile clean &> /dev/null
   ./make_cfast.sh &> $OUTPUT_DIR/stage2
 }

check_compile_cfast_db()
{
   # Check for errors in CFAST debug compilation
   cd $cfastrepo/CFAST/intel_linux_64_db
   if [ -e "cfast7_linux_64_db" ]
   then
      stage2_success=true
   else
      echo "Errors from Stage 2 - Compile CFAST debug:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage2 >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 2 - Compile CFAST debug:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2 >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ========================================
#  = Stage 3 - Run V&V cases (debug mode) =
#  ========================================

wait_vv_cases_debug_start()
{
   # Scans qstat and waits for V&V cases to start
   while [[ `qstat -a | grep $(whoami) | grep Q` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep $JOBPREFIX | grep Q | wc -l`
      echo "Waiting for ${JOBS_REMAINING} V&V cases to start." >> $OUTPUT_DIR/stage3
      TIME_LIMIT_STAGE="3"
      check_time_limit
      sleep 30
   done
}

wait_vv_cases_debug_end()
{
   # Scans qstat and waits for V&V cases to end
   while [[ `qstat -a | grep $(whoami) | grep $JOBPREFIX` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep $JOBPREFIX | wc -l`
      echo "Waiting for ${JOBS_REMAINING} V&V cases to complete." >> $OUTPUT_DIR/stage3
      TIME_LIMIT_STAGE="3"
      check_time_limit
      sleep 30
   done
}

run_vv_cases_debug()
{
   cd $cfastrepo/Validation/scripts

   #  =======================
   #  = Run all cfast cases =
   #  =======================

   # Submit CFAST V&V cases
   echo 'Running CFAST V&V cases:' >> $OUTPUT_DIR/stage3 2>&1
   ./Run_CFAST_Cases.sh -m 2 -d -q $CFASTBOT_QUEUE >> $OUTPUT_DIR/stage3 2>&1
   wait_vv_cases_debug_start

   # Wait for V&V cases to end
   wait_vv_cases_debug_end
}

check_vv_cases_debug()
{
   # Scan and report any errors in CFAST Verification cases
   cd $cfastrepo/Verification

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage3` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage3 >> $OUTPUT_DIR/stage3_errors
      grep "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage3_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage3_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage3_errors
      
      echo "Errors from Stage 3 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage3_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd $cfastrepo/Validation

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage3` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage3 >> $OUTPUT_DIR/stage3_errors
      grep "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage3_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage3_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage3_errors
      
      echo "Errors from Stage 3 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage3_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   #  =====================
   #  = Remove case files =
   #  =====================

   # Remove all unversioned case files from V&V directories (recursively)
   git clean -dxf
}

#  ===================================
#  = Stage 4 - Compile CFAST release =
#  ===================================

compile_cfast()
{ 
   # Build release CFAST
   cd $cfastrepo/CFAST/intel_linux_64
   make -f ../makefile clean &> /dev/null
   ./make_cfast.sh &> $OUTPUT_DIR/stage4
}

check_compile_cfast()
{
   # Check for errors in CFAST release compilation
   cd $cfastrepo/CFAST/intel_linux_64
   if [[ -e "cfast7_linux_64" ]]
   then
      stage4_success=true
   else
      echo "Errors from Stage 4 - Compile CFAST:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage4 >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage4` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 4 - Compile CFAST release:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage4 >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ==========================================
#  = Stage 5 - Run V&V cases (release mode) =
#  ==========================================

wait_vv_cases_release_end()
{
   # Scans qstat and waits for V&V cases to end
   while [[ `qstat -a | grep $(whoami) | grep $JOBPREFIX` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep $JOBPREFIX | wc -l`
      echo "Waiting for ${JOBS_REMAINING} V&V cases to complete." >> $OUTPUT_DIR/stage5
      TIME_LIMIT_STAGE="5"
      check_time_limit
      sleep 60
   done
}

run_vv_cases_release()
{
   # Start running all CFAST V&V cases
   cd $cfastrepo/Validation/scripts
   echo 'Running CFAST V&V cases:' >> $OUTPUT_DIR/stage5 2>&1
   ./Run_CFAST_Cases.sh -q $CFASTBOT_QUEUE >> $OUTPUT_DIR/stage5 2>&1

   # Wait for all V&V cases to end
   wait_vv_cases_release_end
}

check_vv_cases_release()
{
   # Scan and report any errors in CFAST Verificaion cases
   cd $cfastrepo/Verification

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage5` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage5 >> $OUTPUT_DIR/stage5_errors
      grep "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      
      echo "Errors from Stage 5 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage5_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd $cfastrepo/Validation

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${OUTPUT_DIR}/stage5` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $OUTPUT_DIR/stage5 >> $OUTPUT_DIR/stage5_errors
      grep "***Error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $OUTPUT_DIR/stage5_errors
      
      echo "Errors from Stage 5 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage5_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi
}

#  ====================================
#  = Stage 6a - Compile SMV utilities =
#  ====================================

compile_smv_utilities()
{  
   # smokeview libraries
   cd $fdsrepo/SMV/Build/LIBS/lib_linux_intel_64
   echo 'Building Smokeview libraries:' >> $OUTPUT_DIR/stage6a 2>&1
   ./makelibs.sh >> $OUTPUT_DIR/stage6a 2>&1

   # smokezip:
   cd $fdsrepo/Utilities/smokezip/intel_linux_64
   echo 'Compiling smokezip:' >> $OUTPUT_DIR/stage6a 2>&1
   ./make_zip.sh >> $OUTPUT_DIR/stage6a 2>&1
   echo "" >> $OUTPUT_DIR/stage6a 2>&1
   
   # smokediff:
   cd $fdsrepo/Utilities/smokediff/intel_linux_64
   echo 'Compiling smokediff:' >> $OUTPUT_DIR/stage6a 2>&1
   ./make_diff.sh >> $OUTPUT_DIR/stage6a 2>&1
   echo "" >> $OUTPUT_DIR/stage6a 2>&1
   
   # background:
   cd $fdsrepo/Utilities/background/intel_linux_64
   echo 'Compiling background:' >> $OUTPUT_DIR/stage6a 2>&1
   ./make_background.sh >> $OUTPUT_DIR/stage6a 2>&1
}

check_smv_utilities()
{
   # Check for errors in SMV utilities compilation
   cd $FDS_GITTOOT
   if [ -e "$fdsrepo/Utilities/smokezip/intel_linux_64/smokezip_linux_64" ]  && \
      [ -e "$fdsrepo/Utilities/smokediff/intel_linux_64/smokediff_linux_64" ]  && \
      [ -e "$fdsrepo/Utilities/background/intel_linux_64/background" ]
   then
      stage6a_success=true
   else
      echo "Errors from Stage 6a - Compile SMV utilities:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage6a >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#  =============================
#  = Stage 6b - Compile SMV DB =
#  =============================

compile_smv_db()
{
   # Clean and compile SMV DB
   cd $fdsrepo/SMV/Build/intel_linux_64
   ./make_smv_db.sh &> $OUTPUT_DIR/stage6b
}

check_compile_smv_db()
{
   # Check for errors in SMV DB compilation
   cd $fdsrepo/SMV/Build/intel_linux_64
   if [ -e "smokeview_linux_64_db" ]
   then
      stage6b_success=true
   else
      echo "Errors from Stage 6b - Compile SMV DB:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage6b >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage6b | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
   then
      # Continue along
      :
   else
      echo "Stage 6b warnings:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage6b | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  =============================================
#  = Stage 6c - Make SMV pictures (debug mode) =
#  =============================================

# make_cfast_pictures_db()
# {
#    # Run Make SMV Pictures script (debug mode)
#    cd $cfastrepo/Verification/scripts
#    ./Make_SMV_Pictures.sh -d 2>&1 | grep -v FreeFontPath &> $OUTPUT_DIR/stage6c
# }

# check_cfast_pictures_db()
# {
#    # Scan and report any errors in make SMV pictures process
#    cd $CFASTBOT_RUNDIR
#    if [[ `grep -B 50 -A 50 "Segmentation" -I $OUTPUT_DIR/stage6c` == "" && `grep "*** Error" -I $OUTPUT_DIR/stage6c` == "" ]]
#    then
#       stage6c_success=true
#    else
#       cp $OUTPUT_DIR/stage6c $OUTPUT_DIR/stage6c_errors
#       echo "Errors from Stage 6c - Make SMV pictures (debug mode):" >> $ERROR_LOG
#       cat $OUTPUT_DIR/stage6c_errors >> $ERROR_LOG
#       echo "" >> $ERROR_LOG
#    fi
# }

#  ==================================
#  = Stage 6d - Compile SMV release =
#  ==================================

compile_smv()
{
   # Clean and compile SMV
   cd $fdsrepo/SMV/Build/intel_linux_64
   ./make_smv.sh &> $OUTPUT_DIR/stage6d
}

check_compile_smv()
{
   # Check for errors in SMV release compilation
   cd $fdsrepo/SMV/Build/intel_linux_64
   if [ -e "smokeview_linux_64" ]
   then
      stage6d_success=true
   else
      echo "Errors from Stage 6d - Compile SMV release:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage6d >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage6d | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
   then
      # Continue along
      :
   else
      echo "Stage 6d warnings:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage6d | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ===============================================
#  = Stage 6e - Make SMV pictures (release mode) =
#  ===============================================

# make_cfast_pictures()
# {
#    # Run Make SMV Pictures script (release mode)
#    cd $cfastrepo/Validatio/scripts
#    ./Make_CFAST_Pictures.sh 2>&1 | grep -v FreeFontPath &> $OUTPUT_DIR/stage6e
# }

# check_cfast_pictures()
# {
#    # Scan and report any errors in make SMV pictures process
#    cd $CFASTBOT_RUNDIR
#    if [[ `grep -B 50 -A 50 "Segmentation" -I $OUTPUT_DIR/stage6e` == "" && `grep "*** Error" -I $OUTPUT_DIR/stage6e` == "" ]]
#    then
#       stage6e_success=true
#    else
#       cp $OUTPUT_DIR/stage6e  $OUTPUT_DIR/stage6e_errors
#       echo "Errors from Stage 6e - Make CFAST pictures (release mode):" >> $ERROR_LOG
#       cat $OUTPUT_DIR/stage6e >> $ERROR_LOG
#       echo "" >> $ERROR_LOG
#    fi
# }

#  ====================
#  = Stage 7 - Matlab =
#  ====================

# Functions to check for an available Matlab license

run_matlab_license_test()
{
   # Run simple test to see if Matlab license is available
   cd $cfastrepo/Utilities/Matlab
   matlab -r "try, disp('Running Matlab License Check'), catch, disp('License Error'), err = lasterror, err.message, err.stack, end, exit" &> $OUTPUT_DIR/stage7_matlab_license
}

scan_matlab_license_test()
{
   # Check for failed license
   if [[ `grep "License checkout failed" $OUTPUT_DIR/stage7_matlab_license` == "" ]]
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

#  =============================================
#  = Stage 7a - Matlab plotting (verification) =
#  =============================================

run_matlab_verification()
{
   # Run Matlab plotting script
   cd $cfastrepo/Utilities/Matlab

   matlab -r "try, disp('Running Matlab Verification script'), CFAST_verification_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $OUTPUT_DIR/stage7a_verification
}

check_matlab_verification()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_RUNDIR

   if [[ `grep -A 50 "Error" $OUTPUT_DIR/stage7a_verification` == "" ]]
   then
      stage7a_success=true
   else
      grep -A 50 "Error" $OUTPUT_DIR/stage7a_verification >> $OUTPUT_DIR/stage7a_warnings

      echo "Warnings from Stage 7a - Matlab plotting (verification):" >> $WARNING_LOG
      cat $OUTPUT_DIR/stage7a_warnings >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ==========================================================
#  = Stage 7c - Matlab plotting and statistics (validation) =
#  ==========================================================

run_matlab_validation()
{
   # Run Matlab plotting script
   cd $cfastrepo/Utilities/Matlab
   matlab -r "try, disp('Running Matlab Validation script'), CFAST_validation_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $OUTPUT_DIR/stage7c_validation
}

check_matlab_validation()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_RUNDIR
   if [[ `grep -A 50 "Error" $OUTPUT_DIR/stage7c_validation` == "" ]]
   then
      stage7b_success=true
   else
      grep -A 50 "Error" $OUTPUT_DIR/stage7c_validation >> $OUTPUT_DIR/stage7c_warnings

      echo "Warnings from Stage 7c - Matlab plotting and statistics (validation):" >> $WARNING_LOG
      cat $OUTPUT_DIR/stage7b_warnings >> $WARNING_LOG
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
         echo "Baseline validation statistics vs. Revision ${git_REVISION}:" >> $VALIDATION_STATS_LOG
         echo "-------------------------------" >> $VALIDATION_STATS_LOG
         head -n 1 ${BASELINE_STATS_FILE} >> $VALIDATION_STATS_LOG
         echo "" >> $VALIDATION_STATS_LOG
         diff -u <(sed 's/"//g' ${BASELINE_STATS_FILE}) <(sed 's/"//g' ${CURRENT_STATS_FILE}) >> $VALIDATION_STATS_LOG
         echo "" >> $VALIDATION_STATS_LOG
      fi
   else
      echo "Warnings from Stage 7b - Matlab plotting and statistics (validation):" >> $WARNING_LOG
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
      cp ${CURRENT_STATS_FILE} "$CFASTBOT_RUNDIR/history/${STATS_FILE_BASENAME}_${git_REVISION}.csv"

      # Copy to web results
      cp ${CURRENT_STATS_FILE} /var/www/html/cfastbot/manuals/Validation_Statistics/${STATS_FILE_BASENAME}_${git_REVISION}.csv
   fi
}

#  ================================
#  = Stage 8 - Build CFAST Guides =
#  ================================

check_guide()
{
   # Scan and report any errors or warnings in build process for guides
   cd $CFASTBOT_RUNDIR
   if [[ `grep -I "successfully" $1` == "" ]]
   then
      # There were errors/warnings in the guide build process
      echo "Warnings from Stage 8 - Build CFAST Guides:" >> $WARNING_LOG
      echo $3 >> $WARNING_LOG # Name of guide
      cat $1 >> $WARNING_LOG # Contents of log file
      echo "" >> $WARNING_LOG
   else
      # Guide built successfully; there were no errors/warnings
      # Copy guide to CFASTbot's local website
      cp $2 /var/www/html/cfastbot/manuals/
   fi
}

make_cfast_tech_guide()
{
   # Build CFAST tech Guide
   cd $cfastrepo/Docs/Tech_Ref
   ./make_guide.sh &> $OUTPUT_DIR/stage8_cfast_tech_guide

   # Check guide for completion and copy to website if successful
   check_guide $OUTPUT_DIR/stage8_cfast_tech_guide $cfastrepo/Docs/Tech_Ref/Tech_Ref.pdf 'CFAST Technical Reference Guide'
}

make_cfast_vv_guide()
{
   # Build CFAST tech Guide
   cd $cfastrepo/Docs/Validation_Guide
   ./make_guide.sh &> $OUTPUT_DIR/stage8_cfast_vv_guide

   # Check guide for completion and copy to website if successful
   check_guide $OUTPUT_DIR/stage8_cfast_vv_guide $cfastrepo/Docs/Validation_Guide/Validation_Guide.pdf 'CFAST Verification and Validation Guide'
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
     echo "Build failure and warnings for Revision ${git_REVISION}." > "$CFASTBOT_RUNDIR/history/${git_REVISION}.txt"
     cat $ERROR_LOG > "$CFASTBOT_RUNDIR/history/${git_REVISION}_errors.txt"
     touch $OUTPUT_DIR/status_errors_and_warnings

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
      echo "Build failure for Revision ${git_REVISION}." > "$CFASTBOT_RUNDIR/history/${git_REVISION}.txt"
      cat $ERROR_LOG > "$CFASTBOT_RUNDIR/history/${git_REVISION}_errors.txt"
      touch $OUTPUT_DIR/status_errors

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
      echo "Revision ${git_REVISION} has warnings." > "$CFASTBOT_RUNDIR/history/${git_REVISION}.txt"
      cat $WARNING_LOG > "$CFASTBOT_RUNDIR/history/${git_REVISION}_warnings.txt"
      touch $OUTPUT_DIR/status_warnings

   # No errors or warnings
   else
      echo "Build success! Revision ${git_REVISION} passed all build tests." > "$CFASTBOT_RUNDIR/history/${git_REVISION}.txt"
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
      echo "The current git revision is ${git_REVISION}" >> $TIME_LOG
   fi
   echo "-------------------------------" >> $TIME_LOG
   echo "Host: $hostname " >> $TIME_LOG
   echo "Start Time: $start_time " >> $TIME_LOG
   echo "Stop Time: $stop_time " >> $TIME_LOG
   echo "-------------------------------" >> $TIME_LOG
   echo "Nightly Manuals (public): https://drive.google.com/folderview?id=0B_wB1pJL2bFQSkhyNDJ0bEw0cVE#list" >> $TIME_LOG
   echo "-------------------------------" >> $TIME_LOG
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
     mail -s "CFASTbot build failure and warnings on ${hostname}. Revision ${git_REVISION}." $mailTo < $ERROR_LOG > /dev/null

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
     cat $TIME_LOG >> $ERROR_LOG
      # Send email with failure message, body of email contains error log file
      mail -s "CFASTbot build failure on ${hostname}. Revision ${git_REVISION}." $mailTo < $ERROR_LOG > /dev/null

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
     cat $TIME_LOG >> $WARNING_LOG
      # Send email with success message, include warnings
      mail -s "CFASTbot build success with warnings on ${hostname}. Revision ${git_REVISION}." $mailTo < $WARNING_LOG > /dev/null

   # No errors or warnings
   else
      # Send empty email with success message
      mail -s "CFASTbot build success on ${hostname}! Revision ${git_REVISION}." $mailTo < $TIME_LOG > /dev/null
   fi

   # Send email notification if validation statistics have changed.
   if [ -e $VALIDATION_STATS_LOG ]
   then
      mail -s "CFASTbot notice. Validation statistics have changed for Revision ${git_REVISION}." $mailTo < $VALIDATION_STATS_LOG > /dev/null      
   fi
}

# if -a option is invoked, only proceed running CFASTbot if the smokeview or CFAST source has changed

if [[ $RUNAUTO == "y" ]] ; then
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

### Stage 2 ###
compile_cfast_db
check_compile_cfast_db

### Stage 3 ###
if [[ $stage2_success ]] ; then
   run_vv_cases_debug
   check_vv_cases_debug
fi

### Stage 4 ###
compile_cfast
check_compile_cfast

### Stage 6a ###
compile_smv_utilities
check_smv_utilities

### Stage 5 ###
if [[ $stage4_success ]] ; then
   run_vv_cases_release
   check_vv_cases_release
fi

### Stage 6b ###
compile_smv_db
check_compile_smv_db

### Stage 6c ###
if [[ $stage4_success && $stage6b_success ]] ; then
   make_cfast_pictures_db
   check_cfast_pictures_db
fi

### Stage 6d ###
compile_smv
check_compile_smv

### Stage 6e ###
# if [[ $stage4a_success && $stage6d_success ]] ; then
#    make_cfast_pictures
#    check_cfast_pictures
# fi


### Stage 7a ###
if [[ "$SKIP" == "" ]]; then
  check_matlab_license_server
  run_matlab_verification
  check_matlab_verification
fi

### Stage 7c ###
if [[ "$SKIP" == "" ]]; then
  run_matlab_validation
  check_matlab_validation
  check_validation_stats
  archive_validation_stats
fi

### Stage 8 ###
if [[ "$SKIP" == "" ]]; then
  make_cfast_tech_guide
  make_cfast_vv_guide
fi

### Report results ###
set_files_world_readable
save_build_status
email_build_status

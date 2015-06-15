#!/bin/bash

# CFASTbot
# This script is a simplified version of Kris Overholt's Firebot script.
# It runs the CFAST verification/validation suite on the latest
# revision of the repository.

#  ===================
#  = Input variables =
#  ===================

mailTo="gforney@gmail.com, rpeacoc@nist.gov"

CFASTBOT_QUEUE=smokebot
RUNAUTO=
LABEL=
while getopts 'al:q:s' OPTION
do
case $OPTION in
   a)
     RUNAUTO="y"
     ;;
   l)
     LABEL="$OPTARG"
     ;;
   q)
     CFASTBOT_QUEUE="$OPTARG"
     ;;
   s)
     SKIP_git_UPDATE_AND_PROPFIX=true
     ;;
esac
done
shift $(($OPTIND-1))

cd
CFASTBOT_HOME_DIR="`pwd`"
CFASTBOT_DIR="$CFASTBOT_HOME_DIR/cfastbot"

FDS_GITBASE=FDS-SMVgitclean
export FDS_gitROOT="$CFASTBOT_HOME_DIR/$FDS_GITBASE"

CFAST_GITBASE=cfastgitclean
CFAST_GITROOT="$CFASTBOT_HOME_DIR/$CFAST_GITBASE"

ERROR_LOG=$CFASTBOT_DIR/output/errors
TIME_LOG=$CFASTBOT_DIR/output/timings
WARNING_LOG=$CFASTBOT_DIR/output/warnings
VALIDATION_STATS_LOG=$CFASTBOT_DIR/output/statistics
git_LOG=$CFASTBOT_HOME_DIR/git_LOG
RUNNING=$CFASTBOT_DIR/running
export TEXINPUTS=".:../LaTeX_Style_Files:"

THIS_CFAST_FAILED=0
CFAST_STATUS_FILE=$git_LOG/cfast_status
LAST_CFAST_FAILED=0
if [ -e $CFAST_STATUS_FILE ] ; then
   LAST_CFAST_FAILED=`cat $CFAST_STATUS_FILE`
fi
touch $RUNNING

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
   SMV_SOURCE=$FDS_gitROOT/SMV/source
   git_SMVFILE=$git_LOG/smokeview_source_revision
   git_SMVLOG=$git_LOG/smokeview_source_log

   CFAST_SOURCE=$CFAST_GITROOT/CFAST/Source
   git_CFASTSOURCEFILE=$git_LOG/cfast_source_revision
   git_CFASTSOURCELOG=$git_LOG/cfast_source_log
  
   CFAST_DOCS=$CFAST_GITROOT/Docs
   git_CFASTDOCSFILE=$git_LOG/cfast_docs_revision
   git_CFASTDOCSLOG=$git_LOG/cfast_docs_log

   SMOKEBOTDIR=~/CFASTBOT/
   SMOKEBOTEXE=./run_cfastbot.sh

   MESSAGE_FILE=$git_LOG/message

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
   cd $FDS_gitROOT
   chmod -R go+r *
   cd $CFAST_GITROOT
   chmod -R go+r *
}

clean_cfastbot_history()
{
   # Clean cfastbot metafiles
   cd $CFASTBOT_DIR
   rm output/* > /dev/null
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
   if [ -e "$FDS_gitROOT" ]
   then
      # Revert and clean up temporary unversioned and modified versioned repository files
      cd $FDS_gitROOT
      git clean -dxf > /dev/null
      git add . > /dev/null
      git reset --hard HEAD > /dev/null
   # If not, create FDS repository and checkout
   else
      echo "Downloading FDS repository:" >> $CFASTBOT_DIR/output/stage1 2>&1
      cd $CFASTBOT_HOME_DIR
      git clone git@github.com:firemodels/fds-smv.git $FDS_GITBASE >> $CFASTBOT_DIR/output/stage1 2>&1
   fi
   
   # Check to see if CFAST repository exists
   if [ -e "$CFAST_GITROOT" ]
   then
      # Revert and clean up temporary unversioned and modified versioned repository files
      cd $CFAST_GITROOT
      git clean -dxf > /dev/null
      git add . > /dev/null
      git reset --hard HEAD > /dev/null
   # If not, create CFAST repository and checkout
   else
      echo "Downloading CFAST repository:" >> $CFASTBOT_DIR/output/stage1 2>&1
      cd $CFASTBOT_HOME_DIR
      git clone git@github.com:firemodels/cfast.git $CFAST_GITBASE >> $CFASTBOT_DIR/output/stage1 2>&1
   fi
}

do_git_checkout()
{
   cd $FDS_gitROOT
   echo "Checking out latest FDS-SMV revision." >> $CFASTBOT_DIR/output/stage1 2>&1
   git pull >> $CFASTBOT_DIR/output/stage1 2>&1

   cd $CFAST_GITROOT
   echo "Checking out latest CFAST revision." >> $CFASTBOT_DIR/output/stage1 2>&1
   git pull >> $CFASTBOT_DIR/output/stage1 2>&1
   git_REVISION=`tail -n 1 $CFASTBOT_DIR/output/stage1 | sed "s/[^0-9]//g"`
}

check_git_checkout()
{
   # Check for git errors
   stage1_success=true
}

fix_svn_properties()
{
   # This function fixes git properties
   # (e.g., svn:executable, svn:keywords, svn:eol-style, and svn:mime-type)
   # throughout the CFAST repository.

   cd $CFAST_GITROOT

   # Delete all svn:executable properties
   svn propdel svn:executable --recursive &> /dev/null

   # Restore local executable property to svn-fix-props.pl
   chmod +x Utilities/Subversion/svn-fix-props.pl &> /dev/null

   # Run svn-fix-props.pl script (fixes all git properties)
   Utilities/Subversion/svn-fix-props.pl ./ &> /dev/null

   # Commit back results
   svn commit -m 'CFASTbot: Fix git properties throughout repository' &> /dev/null
}

print_svn_revision_on_skip()
{
   # Prints log output and git revision number when SKIP_git_UPDATE_AND_PROPFIX option is used
   cd $CFAST_GITROOT
   git_REVISION=`git log --abbrev-commit . | head -1 | awk '{print $2}'`

   echo "CFASTbot was invoked with the -s option (SKIP_git_UPDATE_AND_PROPFIX)." >> $CFASTBOT_DIR/output/stage1 2>&1
   echo "Skipping git revert, update, and property fix operations." >> $CFASTBOT_DIR/output/stage1 2>&1
   echo "The current git revision is ${git_REVISION}" >> $CFASTBOT_DIR/output/stage1 2>&1
}

#  =================================
#  = Stage 2 - Compile CFAST debug =
#  =================================

compile_cfast_db()
{
   # Build debug CFAST
   cd $CFAST_GITROOT/CFAST/intel_linux_64_db
   make -f ../makefile clean &> /dev/null
   ./make_cfast.sh &> $CFASTBOT_DIR/output/stage2
 }

check_compile_cfast_db()
{
   # Check for errors in CFAST debug compilation
   cd $CFAST_GITROOT/CFAST/intel_linux_64_db
   if [ -e "cfast7_linux_64_db" ]
   then
      stage2_success=true
   else
      echo "Errors from Stage 2 - Compile CFAST debug:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage2 >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage2` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 2 - Compile CFAST debug:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage2 >> $WARNING_LOG
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
      echo "Waiting for ${JOBS_REMAINING} V&V cases to start." >> $CFASTBOT_DIR/output/stage3
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
      echo "Waiting for ${JOBS_REMAINING} V&V cases to complete." >> $CFASTBOT_DIR/output/stage3
      TIME_LIMIT_STAGE="3"
      check_time_limit
      sleep 30
   done
}

run_vv_cases_debug()
{
   cd $CFAST_GITROOT/Validation/scripts

   #  =======================
   #  = Run all cfast cases =
   #  =======================

   # Submit CFAST V&V cases
   echo 'Running CFAST V&V cases:' >> $CFASTBOT_DIR/output/stage3 2>&1
   ./Run_CFAST_Cases.sh -d -q $CFASTBOT_QUEUE >> $CFASTBOT_DIR/output/stage3 2>&1
   wait_vv_cases_debug_start

   # Wait for V&V cases to end
   wait_vv_cases_debug_end
}

check_vv_cases_debug()
{
   # Scan and report any errors in CFAST Verification cases
   cd $CFAST_GITROOT/Verification

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${CFASTBOT_DIR}/output/stage3` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $CFASTBOT_DIR/output/stage3 >> $CFASTBOT_DIR/output/stage3_errors
      grep "***Error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage3_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage3_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage3_errors
      
      echo "Errors from Stage 3 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage3_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd $CFAST_GITROOT/Validation

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${CFASTBOT_DIR}/output/stage3` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $CFASTBOT_DIR/output/stage3 >> $CFASTBOT_DIR/output/stage3_errors
      grep "***Error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage3_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage3_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage3_errors
      
      echo "Errors from Stage 3 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage3_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   #  =====================
   #  = Remove case files =
   #  =====================

   # Remove all unversioned case files from V&V directories (recursively)
   cd $CFAST_GITROOT/Verification
   git clean -dxf
   
   cd $CFAST_GITROOT/Validation
   git clean -dxf
}

#  ===================================
#  = Stage 4 - Compile CFAST release =
#  ===================================

compile_cfast()
{ 
   # Build release CFAST
   cd $CFAST_GITROOT/CFAST/intel_linux_64
   make -f ../makefile clean &> /dev/null
   ./make_cfast.sh &> $CFASTBOT_DIR/output/stage4
}

check_compile_cfast()
{
   # Check for errors in CFAST release compilation
   cd $CFAST_GITROOT/CFAST/intel_linux_64
   if [[ -e "cfast7_linux_64" ]]
   then
      stage4_success=true
   else
      echo "Errors from Stage 4 - Compile CFAST:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage4 >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage4` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 4 - Compile CFAST release:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage4 >> $WARNING_LOG
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
      echo "Waiting for ${JOBS_REMAINING} V&V cases to complete." >> $CFASTBOT_DIR/output/stage5
      TIME_LIMIT_STAGE="5"
      check_time_limit
      sleep 60
   done
}

run_vv_cases_release()
{
   # Start running all CFAST V&V cases
   cd $CFAST_GITROOT/Validation/scripts
   echo 'Running CFAST V&V cases:' >> $CFASTBOT_DIR/output/stage5 2>&1
   ./Run_CFAST_Cases.sh -q $CFASTBOT_QUEUE >> $CFASTBOT_DIR/output/stage5 2>&1

   # Wait for all V&V cases to end
   wait_vv_cases_release_end
}

check_vv_cases_release()
{
   # Scan and report any errors in CFAST Verificaion cases
   cd $CFAST_GITROOT/Verification

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${CFASTBOT_DIR}/output/stage5` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $CFASTBOT_DIR/output/stage5 >> $CFASTBOT_DIR/output/stage5_errors
      grep "***Error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage5_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage5_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage5_errors
      
      echo "Errors from Stage 5 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage5_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd $CFAST_GITROOT/Validation

   if [[ `grep 'Run aborted' -riI --include *.log --include *.err ${CFASTBOT_DIR}/output/stage5` == "" ]] && \
      [[ `grep "***Error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep "***Fatal error" -riI --include *.log --include *.err *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include *.log --include *.err *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include *.log --include *.err $CFASTBOT_DIR/output/stage5 >> $CFASTBOT_DIR/output/stage5_errors
      grep "***Error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage5_errors
      grep "***Fatal error" -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage5_errors
      grep -A 20 forrtl -riI --include *.log --include *.err * >> $CFASTBOT_DIR/output/stage5_errors
      
      echo "Errors from Stage 5 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage5_errors >> $ERROR_LOG
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
   cd $FDS_gitROOT/SMV/Build/LIBS/lib_linux_intel_64
   echo 'Building Smokeview libraries:' >> $CFASTBOT_DIR/output/stage6a 2>&1
   ./makelibs.sh >> $CFASTBOT_DIR/output/stage6a 2>&1

   # smokezip:
   cd $FDS_gitROOT/Utilities/smokezip/intel_linux_64
   echo 'Compiling smokezip:' >> $CFASTBOT_DIR/output/stage6a 2>&1
   ./make_zip.sh >> $CFASTBOT_DIR/output/stage6a 2>&1
   echo "" >> $CFASTBOT_DIR/output/stage6a 2>&1
   
   # smokediff:
   cd $FDS_gitROOT/Utilities/smokediff/intel_linux_64
   echo 'Compiling smokediff:' >> $CFASTBOT_DIR/output/stage6a 2>&1
   ./make_diff.sh >> $CFASTBOT_DIR/output/stage6a 2>&1
   echo "" >> $CFASTBOT_DIR/output/stage6a 2>&1
   
   # background:
   cd $FDS_gitROOT/Utilities/background/intel_linux_32
   echo 'Compiling background:' >> $CFASTBOT_DIR/output/stage6a 2>&1
   ./make_background.sh >> $CFASTBOT_DIR/output/stage6a 2>&1
}

check_smv_utilities()
{
   # Check for errors in SMV utilities compilation
   cd $FDS_gitROOT
   if [ -e "$FDS_gitROOT/Utilities/smokezip/intel_linux_64/smokezip_linux_64" ]  && \
      [ -e "$FDS_gitROOT/Utilities/smokediff/intel_linux_64/smokediff_linux_64" ]  && \
      [ -e "$FDS_gitROOT/Utilities/background/intel_linux_32/background" ]
   then
      stage6a_success=true
   else
      echo "Errors from Stage 6a - Compile SMV utilities:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage6a >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#  =============================
#  = Stage 6b - Compile SMV DB =
#  =============================

compile_smv_db()
{
   # Clean and compile SMV DB
   cd $FDS_gitROOT/SMV/Build/intel_linux_64
   ./make_smv_db.sh &> $CFASTBOT_DIR/output/stage6b
}

check_compile_smv_db()
{
   # Check for errors in SMV DB compilation
   cd $FDS_gitROOT/SMV/Build/intel_linux_64
   if [ -e "smokeview_linux_64_db" ]
   then
      stage6b_success=true
   else
      echo "Errors from Stage 6b - Compile SMV DB:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage6b >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
   if [[ `grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage6b | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
   then
      # Continue along
      :
   else
      echo "Stage 6b warnings:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage6b | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  =============================================
#  = Stage 6c - Make SMV pictures (debug mode) =
#  =============================================

# make_cfast_pictures_db()
# {
#    # Run Make SMV Pictures script (debug mode)
#    cd $CFAST_GITROOT/Verification/scripts
#    ./Make_SMV_Pictures.sh -d 2>&1 | grep -v FreeFontPath &> $CFASTBOT_DIR/output/stage6c
# }

# check_cfast_pictures_db()
# {
#    # Scan and report any errors in make SMV pictures process
#    cd $CFASTBOT_DIR
#    if [[ `grep -B 50 -A 50 "Segmentation" -I $CFASTBOT_DIR/output/stage6c` == "" && `grep "*** Error" -I $CFASTBOT_DIR/output/stage6c` == "" ]]
#    then
#       stage6c_success=true
#    else
#       cp $CFASTBOT_DIR/output/stage6c $CFASTBOT_DIR/output/stage6c_errors
#       echo "Errors from Stage 6c - Make SMV pictures (debug mode):" >> $ERROR_LOG
#       cat $CFASTBOT_DIR/output/stage6c_errors >> $ERROR_LOG
#       echo "" >> $ERROR_LOG
#    fi
# }

#  ==================================
#  = Stage 6d - Compile SMV release =
#  ==================================

compile_smv()
{
   # Clean and compile SMV
   cd $FDS_gitROOT/SMV/Build/intel_linux_64
   ./make_smv.sh &> $CFASTBOT_DIR/output/stage6d
}

check_compile_smv()
{
   # Check for errors in SMV release compilation
   cd $FDS_gitROOT/SMV/Build/intel_linux_64
   if [ -e "smokeview_linux_64" ]
   then
      stage6d_success=true
   else
      echo "Errors from Stage 6d - Compile SMV release:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage6d >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
   if [[ `grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage6d | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
   then
      # Continue along
      :
   else
      echo "Stage 6d warnings:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${CFASTBOT_DIR}/output/stage6d | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ===============================================
#  = Stage 6e - Make SMV pictures (release mode) =
#  ===============================================

# make_cfast_pictures()
# {
#    # Run Make SMV Pictures script (release mode)
#    cd $CFAST_GITROOT/Validatio/scripts
#    ./Make_CFAST_Pictures.sh 2>&1 | grep -v FreeFontPath &> $CFASTBOT_DIR/output/stage6e
# }

# check_cfast_pictures()
# {
#    # Scan and report any errors in make SMV pictures process
#    cd $CFASTBOT_DIR
#    if [[ `grep -B 50 -A 50 "Segmentation" -I $CFASTBOT_DIR/output/stage6e` == "" && `grep "*** Error" -I $CFASTBOT_DIR/output/stage6e` == "" ]]
#    then
#       stage6e_success=true
#    else
#       cp $CFASTBOT_DIR/output/stage6e  $CFASTBOT_DIR/output/stage6e_errors
#       echo "Errors from Stage 6e - Make CFAST pictures (release mode):" >> $ERROR_LOG
#       cat $CFASTBOT_DIR/output/stage6e >> $ERROR_LOG
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
   cd $CFAST_GITROOT/Utilities/Matlab
   matlab -r "try, disp('Running Matlab License Check'), catch, disp('License Error'), err = lasterror, err.message, err.stack, end, exit" &> $CFASTBOT_DIR/output/stage7_matlab_license
}

scan_matlab_license_test()
{
   # Check for failed license
   if [[ `grep "License checkout failed" $CFASTBOT_DIR/output/stage7_matlab_license` == "" ]]
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
   cd $CFAST_GITROOT/Utilities/Matlab

   matlab -r "try, disp('Running Matlab Verification script'), CFAST_verification_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $CFASTBOT_DIR/output/stage7a_verification
}

check_matlab_verification()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_DIR

   if [[ `grep -A 50 "Error" $CFASTBOT_DIR/output/stage7a_verification` == "" ]]
   then
      stage7a_success=true
   else
      grep -A 50 "Error" $CFASTBOT_DIR/output/stage7a_verification >> $CFASTBOT_DIR/output/stage7a_warnings

      echo "Warnings from Stage 7a - Matlab plotting (verification):" >> $WARNING_LOG
      cat $CFASTBOT_DIR/output/stage7a_warnings >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  =============================================
#  = Stage 7b - Matlab plotting (verification) =
#  =============================================

run_matlab_plotting()
{
   # Run Matlab plotting script
   cd $CFAST_GITROOT/Utilities/Matlab

   matlab -r "try, disp('Running Matlab Plotting script'), CFAST_plotting_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $CFASTBOT_DIR/output/stage7b_verification
}

check_matlab_plotting()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_DIR

   if [[ `grep -A 50 "Error" $CFASTBOT_DIR/output/stage7b_verification` == "" ]]
   then
      stage7a_success=true
   else
      grep -A 50 "Error" $CFASTBOT_DIR/output/stage7b_verification >> $CFASTBOT_DIR/output/stage7b_warnings

      echo "Warnings from Stage 7b - Matlab plotting (verification 2):" >> $WARNING_LOG
      cat $CFASTBOT_DIR/output/stage7b_warnings >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#  ==========================================================
#  = Stage 7c - Matlab plotting and statistics (validation) =
#  ==========================================================

run_matlab_validation()
{
   # Run Matlab plotting script
   cd $CFAST_GITROOT/Utilities/Matlab
   matlab -r "try, disp('Running Matlab Validation script'), CFAST_validation_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $CFASTBOT_DIR/output/stage7c_validation
}

check_matlab_validation()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_DIR
   if [[ `grep -A 50 "Error" $CFASTBOT_DIR/output/stage7c_validation` == "" ]]
   then
      stage7b_success=true
   else
      grep -A 50 "Error" $CFASTBOT_DIR/output/stage7c_validation >> $CFASTBOT_DIR/output/stage7c_warnings

      echo "Warnings from Stage 7c - Matlab plotting and statistics (validation):" >> $WARNING_LOG
      cat $CFASTBOT_DIR/output/stage7b_warnings >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

check_validation_stats()
{
   cd $CFAST_GITROOT/Utilities/Matlab

   STATS_FILE_BASENAME=CFAST_validation_scatterplot_output

   BASELINE_STATS_FILE=$CFAST_GITROOT/Utilities/Matlab/${STATS_FILE_BASENAME}_baseline.csv
   CURRENT_STATS_FILE=$CFAST_GITROOT/Utilities/Matlab/${STATS_FILE_BASENAME}.csv

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
   cd $CFAST_GITROOT/Utilities/Matlab

   if [ -e ${CURRENT_STATS_FILE} ] ; then
      # Copy to CFASTbot history
      cp ${CURRENT_STATS_FILE} "$CFASTBOT_DIR/history/${STATS_FILE_BASENAME}_${git_REVISION}.csv"
      if [ "$LABEL" != "" ]; then
        cp ${CURRENT_STATS_FILE} "$CFASTBOT_DIR/history/${STATS_FILE_BASENAME}_${git_REVISION}_${LABEL}.csv"
        cp ${CURRENT_STATS_FILE} /var/www/html/cfastbot/manuals/Validation_Statistics/${STATS_FILE_BASENAME}_${git_REVISION}_${LABEL}.csv
      fi

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
   cd $CFASTBOT_DIR
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
   cd $CFAST_GITROOT/Docs/Tech_Ref
   ./make_guide.sh &> $CFASTBOT_DIR/output/stage8_cfast_tech_guide

   # Check guide for completion and copy to website if successful
   check_guide $CFASTBOT_DIR/output/stage8_cfast_tech_guide $CFAST_GITROOT/Docs/Tech_Ref/Tech_Ref.pdf 'CFAST Technical Reference Guide'
}

make_cfast_vv_guide()
{
   # Build CFAST tech Guide
   cd $CFAST_GITROOT/Docs/Validation_Guide
   ./make_guide.sh &> $CFASTBOT_DIR/output/stage8_cfast_vv_guide

   # Check guide for completion and copy to website if successful
   check_guide $CFASTBOT_DIR/output/stage8_cfast_vv_guide $CFAST_GITROOT/Docs/Validation_Guide/Validation_Guide.pdf 'CFAST Verification and Validation Guide'
}

#  =====================================================
#  = Build status reporting - email and save functions =
#  =====================================================

save_build_status()
{
   cd $CFASTBOT_DIR
   # Save status outcome of build to a text file
   if [[ -e $WARNING_LOG && -e $ERROR_LOG ]]
   then
     cat "" >> $ERROR_LOG
     cat $WARNING_LOG >> $ERROR_LOG
     echo "Build failure and warnings for Revision ${git_REVISION}." > "$CFASTBOT_DIR/history/${git_REVISION}.txt"
     cat $ERROR_LOG > "$CFASTBOT_DIR/history/${git_REVISION}_errors.txt"
     touch output/status_errors_and_warnings

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
      echo "Build failure for Revision ${git_REVISION}." > "$CFASTBOT_DIR/history/${git_REVISION}.txt"
      cat $ERROR_LOG > "$CFASTBOT_DIR/history/${git_REVISION}_errors.txt"
      touch output/status_errors

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
      echo "Revision ${git_REVISION} has warnings." > "$CFASTBOT_DIR/history/${git_REVISION}.txt"
      cat $WARNING_LOG > "$CFASTBOT_DIR/history/${git_REVISION}_warnings.txt"
      touch output/status_warnings

   # No errors or warnings
   else
      echo "Build success! Revision ${git_REVISION} passed all build tests." > "$CFASTBOT_DIR/history/${git_REVISION}.txt"
      touch output/status_success
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
   cd $CFASTBOT_DIR
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
if [[ $SKIP_git_UPDATE_AND_PROPFIX ]] ; then
   print_svn_revision_on_skip
else
   clean_git_repo
   do_git_checkout
   check_git_checkout
  # fix_svn_properties
fi

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
# if [[ $stage4a_success && $stage6b_success ]] ; then
#    make_cfast_pictures_db
#    check_cfast_pictures_db
# fi

### Stage 6d ###
compile_smv
check_compile_smv

### Stage 6e ###
# if [[ $stage4a_success && $stage6d_success ]] ; then
#    make_cfast_pictures
#    check_cfast_pictures
# fi

### Stage 7a ###
check_matlab_license_server
run_matlab_verification
check_matlab_verification

### Stage 7b ###
check_matlab_license_server
run_matlab_plotting
check_matlab_plotting

### Stage 7c ###
run_matlab_validation
check_matlab_validation
check_validation_stats
archive_validation_stats

### Stage 8 ###
make_cfast_tech_guide
make_cfast_vv_guide

### Report results ###
set_files_world_readable
save_build_status
email_build_status
rm -f $RUNNING

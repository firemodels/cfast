#!/bin/bash

# cfastbot
# This script is a simplified version of Kris Overholt's firebot script.
# It runs the cfast verification/validation suite (not FDS) on the latest
# revision of the repository.  It does not erase files that are not
# the repository.  This allows one to test working files before they
# have been committed.  

#  ===================
#  = Input variables =
#  ===================

mailTo="gforney@gmail.com, cfast@nist.gov, koverholt@gmail.com"

CFASTBOT_QUEUE=smokebot
RUNAUTO=
while getopts 'aq:s' OPTION
do
case $OPTION in
  a)
   RUNAUTO="y"
   ;;
  q)
   CFASTBOT_QUEUE="$OPTARG"
   ;;
  s)
   SKIP_SVN_PROPS=true
   ;;
esac
done
shift $(($OPTIND-1))

cd
CFASTBOT_HOME_DIR="`pwd`"
CFASTBOT_DIR="$CFASTBOT_HOME_DIR/CFASTBOT"
export FDS_SVNROOT="$CFASTBOT_HOME_DIR/FDS-SMV"
CFAST_SVNROOT="$CFASTBOT_HOME_DIR/cfast"
ERROR_LOG=$CFASTBOT_DIR/output/errors
TIME_LOG=$CFASTBOT_DIR/output/timings
WARNING_LOG=$CFASTBOT_DIR/output/warnings
GUIDE_DIR=$CFASTBOT_DIR/guides
export TEXINPUTS=".:../LaTeX_Style_Files:"

THIS_CFAST_FAILED=0
CFAST_STATUS_FILE=$CFAST_SVNROOT/cfast_status
LAST_CFAST_FAILED=0
if [ -e $CFAST_STATUS_FILE ] ; then
  LAST_CFAST_FAILED=`cat $CFAST_STATUS_FILE`
fi

export JOBPREFIX=CB_

#  =============================================
#  = cfastbot timing and notification mechanism =
#  =============================================

# This routine checks the elapsed time of cfastbot.
# If cfastbot runs more than 12 hours, an email notification is sent.
# This is a notification only and does not terminate cfastbot.
# This check runs during Stages 3 and 5.

# Start cfastbot timer
START_TIME=$(date +%s)

# Set time limit (43,200 seconds = 12 hours)
TIME_LIMIT=43200
TIME_LIMIT_EMAIL_NOTIFICATION="unsent"

run_auto()
{
  SMV_SOURCE=$FDS_SVNROOT/SMV/source
  SVN_SMVFILE=$FDS_SVNROOT/smokeview_source_revision
  SVN_SMVLOG=$FDS_SVNROOT/smokeview_source_log

  CFAST_SOURCE=$CFAST_SVNROOT/CFAST/Source
  SVN_CFASTSOURCEFILE=$FDS_SVNROOT/cfast_source_revision
  SVN_CFASTSOURCELOG=$FDS_SVNROOT/cfast_source_log
  
  CFAST_DOCS=$CFAST_SVNROOT/Docs
  SVN_CFASTDOCSFILE=$CFAST_SVNROOT/cfast_docs_revision
  SVN_CFASTDOCSLOG=$CFAST_SVNROOT/cfast_docs_log

  SMOKEBOTDIR=~/CFASTBOT/
  SMOKEBOTEXE=./run_cfastbot.sh

  MESSAGE_FILE=$CFAST_SVNROOT/message

  cd $SMV_SOURCE
  svn update > /dev/null
  THIS_SMVSVN=`svn info | tail -3 | head -1 | awk '{print $4}'`
  THIS_SMVAUTHOR=`svn info | tail -4 | head -1 | awk '{print $4}'`
  LAST_SMVSVN=`cat $SVN_SMVFILE`
  svn log -r $THIS_SMVSVN > $SVN_SMVLOG

  cd $CFAST_SOURCE
  svn update > /dev/null
  THIS_CFASTSOURCESVN=`svn info | tail -3 | head -1 | awk '{print $4}'`
  THIS_CFASTSOURCEAUTHOR=`svn info | tail -4 | head -1 | awk '{print $4}'`
  LAST_CFASTSOURCESVN=`cat $SVN_CFASTSOURCEFILE`
  svn log -r $THIS_CFASTSOURCESVN > $SVN_CFASTSOURCELOG
  
  cd $CFAST_DOCS
  svn update > /dev/null
  THIS_CFASTDOCSSVN=`svn info | tail -3 | head -1 | awk '{print $4}'`
  THIS_CFASTDOCSAUTHOR=`svn info | tail -4 | head -1 | awk '{print $4}'`
  LAST_CFASTDOCSSVN=`cat $SVN_CFASTDOCSFILE`
  svn log -r $THIS_CFASTDOCSSVN > $SVN_CFASTDOCSLOG
  
  if [[ $THIS_SMVSVN == $LAST_SMVSVN && $THIS_CFASTSOURCESVN == $LAST_CFASTSOURCESVN && $THIS_CFASTDOCSSVN == $LAST_CFASTDOCSSVN ]] ; then
    exit
  fi

  rm -f $MESSAGE_FILE
  if [[ $THIS_SMVSVN != $LAST_SMVSVN ]] ; then
    echo $THIS_SMVSVN>$SVN_SMVFILE
    echo -e "smokeview source has changed. $LAST_SMVSVN->$THIS_SMVSVN($THIS_SMVAUTHOR)" >> $MESSAGE_FILE
    cat $SVN_SMVLOG >> $MESSAGE_FILE
  fi
  if [[ $THIS_CFASTSOURCESVN != $LAST_CFASTSOURCESVN ]] ; then
    echo $THIS_CFASTSOURCESVN>$SVN_CFASTSOURCEFILE
    echo -e "CFAST source directory has changed. $LAST_CFASTSOURCESVN->$THIS_CFASTSOURCE($THIS_CFASTSOURCEAUTHOR)" >> $MESSAGE_FILE
    cat $SVN_CFASTSOURCELOG >> $MESSAGE_FILE
  fi
  if [[ $THIS_CFASTDOCSSVN != $LAST_CFASTDOCSSVN ]] ; then
    echo $THIS_CFASTDOCSSVN>$SVN_CFASTDOCSFILE
    echo -e "CFAST Docs directory has changed. $LAST_CFASTDOCSSVN->$THIS_CFASTDOCS($THIS_CFASTDOCSAUTHOR)" >> $MESSAGE_FILE
    cat $SVN_CFASTDOCSLOG >> $MESSAGE_FILE
  fi
  echo -e "cfastbot run initiated." >> $MESSAGE_FILE
  cat $MESSAGE_FILE | mail -s "cfastbot run initiated" $mailTo > /dev/null
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
         echo -e "cfastbot has been running for more than 12 hours in Stage ${TIME_LIMIT_STAGE}. \n\nPlease ensure that there are no problems. \n\nThis is a notification only and does not terminate cfastbot." | mail -s "cfastbot Notice: cfastbot has been running for more than 12 hours." $mailTo > /dev/null
         TIME_LIMIT_EMAIL_NOTIFICATION="sent"
      fi
   fi
}

#  ========================
#  = Additional functions =
#  ========================

set_files_world_readable()
{
   cd $FDS_SVNROOT
   chmod -R go+r *
   cd $CFAST_SVNROOT
   chmod -R go+r *
}

clean_cfastbot_history()
{
   
   # Clean cfastbot metafiles
   MKDIR $CFASTBOT_DIR
   cd $CFASTBOT_DIR
   MKDIR guides
   MKDIR history
   MKDIR output
   rm -f output/* > /dev/null
}

#  ========================
#  ========================
#  = cfastbot Build Stages =
#  ========================
#  ========================

fix_svn_properties()
{
   # This function fixes SVN properties
   # (e.g., svn:executable, svn:keywords, svn:eol-style, and svn:mime-type)
   # throughout the FDS-SMV repository.

   # cd to SVN root
   cd $CFAST_SVNROOT

   # Delete all svn:executable properties
   svn propdel svn:executable --recursive &> /dev/null

   # Restore local executable property to svn-fix-props.pl
   chmod +x Utilities/Subversion/svn-fix-props.pl &> /dev/null

   # Run svn-fix-props.pl script (fixes all SVN properties)
   Utilities/Subversion/svn-fix-props.pl ./ &> /dev/null

   # Commit back results
   svn commit -m 'Cfastbot: Fix SVN properties throughout repository' &> /dev/null
}

#  ===================================
#  = Stage 0 - External dependencies =
#  ===================================

update_and_compile_cfast()
{
   cd $CFASTBOT_HOME_DIR

   # Check to see if CFAST repository exists
   if [ -e "$CFAST_SVNROOT" ]
   # If yes, then update the CFAST repository and compile CFAST
   then
      echo "Updating and compiling CFAST:" > $CFASTBOT_DIR/output/stage0_cfast
      cd $CFAST_SVNROOT/CFAST
      
      # Update to latest SVN revision
      svn update >> $CFASTBOT_DIR/output/stage0_cfast 2>&1
      
   # If no, then checkout the CFAST repository and compile CFAST
   else
      echo "Downloading and compiling CFAST:" > $CFASTBOT_DIR/output/stage0_cfast
      mkdir -p $CFAST_SVNROOT
      cd $CFAST_SVNROOT

      svn co https://cfast.googlecode.com/svn/trunk/cfast/trunk/CFAST CFAST >> $CFASTBOT_DIR/output/stage0_cfast 2>&1
      
   fi

    # Build debug CFAST

    cd $CFAST_SVNROOT/CFAST/intel_linux_64_db
    rm -f cfast6_linux_64_db
    make --makefile ../makefile clean &> /dev/null
    ./make_cfast.sh >> $CFASTBOT_DIR/output/stage0_cfast 2>&1

   # Check for errors in CFAST debug compilation
   cd $CFAST_SVNROOT/CFAST/intel_linux_64_db
   if [ -e "cfast6_linux_64_db" ]
   then
      stage0_success=true
   fi
 
   # Build release CFAST

    cd $CFAST_SVNROOT/CFAST/intel_linux_64
    rm -f cfast6_linux_64
    make --makefile ../makefile clean &> /dev/null
    ./make_cfast.sh >> $CFASTBOT_DIR/output/stage0_cfast 2>&1

   # Check for errors in CFAST release compilation
   cd $CFAST_SVNROOT/CFAST/intel_linux_64
   if [[ -e "cfast6_linux_64" && stage0_success==true ]]
   then
      stage0_success=true
   else
      echo "Errors from Stage 0 - CFAST:" >> $ERROR_LOG
      echo "CFAST failed to compile" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage0_cfast >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi


}

#  ============================
#  = Stage 1 - SVN operations =
#  ============================

clean_svn_repo()
{
   # Check to see if FDS repository exists
   if [ -e "$FDS_SVNROOT" ]
   then
   # If not, create FDS repository and checkout
     dummy=true
   else
      echo "Downloading FDS repository:" >> $CFASTBOT_DIR/output/stage1a 2>&1
      cd $CFASTBOT_HOME_DIR
      svn co https://fds-smv.googlecode.com/svn/trunk/FDS/trunk/ FDS-SMV >> $CFASTBOT_DIR/output/stage1a 2>&1
   fi
   # Check to see if CFAST repository exists
   if [ -e "$CFAST_SVNROOT" ]
   then
   # If not, create CFAST repository and checkout
     dummy=true
   else
      echo "Downloading CFAST repository:" >> $CFASTBOT_DIR/output/stage1b 2>&1
      cd $CFASTBOT_HOME_DIR
      svn co https://cfast.googlecode.com/svn/trunk/cfast/trunk cfast >> $CFASTBOT_DIR/output/stage1b 2>&1
   fi
}

do_svn_checkout()
{
   cd $FDS_SVNROOT
   echo "Checking out latest FDS-SMV revision." >> $CFASTBOT_DIR/output/stage1a 2>&1
   svn update >> $CFASTBOT_DIR/output/stage1a 2>&1

   cd $CFAST_SVNROOT
   echo "Checking out latest cfast revision." >> $CFASTBOT_DIR/output/stage1b 2>&1
   svn update >> $CFASTBOT_DIR/output/stage1b 2>&1
   SVN_REVISION=`tail -n 1 $CFASTBOT_DIR/output/stage1b | sed "s/[^0-9]//g"`
}

check_svn_checkout()
{
   cd $FDS_SVNROOT
   # Check for SVN errors
   if [[ `grep -E 'Updated|At revision' $CFASTBOT_DIR/output/stage1a | wc -l` -ne 1 ]];
   then
      echo "Errors from Stage 1 - SVN operations:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage1a >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      email_build_status
      exit
   else
      stage1a_success=true
   fi

   cd $CFAST_SVNROOT
   # Check for SVN errors
   if [[ `grep -E 'Updated|At revision' $CFASTBOT_DIR/output/stage1b | wc -l` -ne 1 ]];
   then
      echo "Errors from Stage 1 - SVN operations:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage1b >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      email_build_status
      exit
   else
      stage1b_success=true
   fi
}

wait_verification_cases_short_start()
{
   # Scans qstat and waits for verification cases to start
   while [[ `qstat -a | grep $(whoami) | grep Q` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep $JOBPREFIX | grep Q | wc -l`
      echo "Waiting for ${JOBS_REMAINING} verification cases to start." >> $CFASTBOT_DIR/output/stage3
      TIME_LIMIT_STAGE="3"
      check_time_limit
      sleep 30
   done
}

wait_verification_cases_short_end()
{
   # Scans qstat and waits for verification cases to end
   while [[ `qstat -a | grep $(whoami) | grep $JOBPREFIX` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep $JOBPREFIX | wc -l`
      echo "Waiting for ${JOBS_REMAINING} verification cases to complete." >> $CFASTBOT_DIR/output/stage3
      TIME_LIMIT_STAGE="3"
      check_time_limit
      sleep 30
   done
}

run_verification_cases_short()
{

   #  =====================
   #  = Run all cfast cases =
   #  =====================

   cd $CFAST_SVNROOT/Validation/scripts

   # Submit CFAST verification cases and wait for them to start
   echo 'Running cfast verification cases:' >> $CFASTBOT_DIR/output/stage3 2>&1
   ./Run_CFAST_Cases.sh -d -q $CFASTBOT_QUEUE >> $CFASTBOT_DIR/output/stage3 2>&1
   wait_verification_cases_short_start

   # Wait some additional time for all cases to start
   sleep 30

   # Stop all cases
   ./Run_CFAST_Cases.sh -d -s >> $CFASTBOT_DIR/output/stage3 2>&1
   echo "" >> $CFASTBOT_DIR/output/stage3 2>&1

   # Wait for SMV verification cases to end
   wait_verification_cases_short_end

   #  ======================
   #  = Remove .stop files =
   #  ======================

   # Remove all .stop and .err files from Verification directories (recursively)
   cd $CFAST_SVNROOT/Validation
   find . -name '*.stop' -exec rm -f {} \;
   find . -name '*.err' -exec rm -f {} \;
   find . -name '*.smv' -exec rm -f {} \;
   find . -name '*.out' -exec rm -f {} \;
}

check_verification_cases_short()
{
   # Scan and report any errors in CFAST verification cases
   cd $CFAST_SVNROOT/Validation

   if [[ `grep 'Run aborted' -rI ${CFASTBOT_DIR}/output/stage3` == "" ]] && \
      [[ `grep ERROR: -rI *` == "" ]] && \
      [[ `grep 'STOP: Numerical' -rI *` == "" ]] && \
      [[ `grep -A 20 forrtl -rI *` == "" ]]
   then
      stage3_success=true
   else
      grep 'Run aborted' -rI $CFASTBOT_DIR/output/stage3 > $CFASTBOT_DIR/output/stage3_errors
      grep ERROR: -rI * >> $CFASTBOT_DIR/output/stage3_errors
      grep 'STOP: Numerical' -rI * >> $CFASTBOT_DIR/output/stage3_errors
      grep -A 20 forrtl -rI * >> $CFASTBOT_DIR/output/stage3_errors
      
      echo "Errors from Stage 3 - Run verification cases (short run):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage3_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi
}

#  ===============================================
#  = Stage 5 - Run verification cases (long run) =
#  ===============================================

wait_verification_cases_long_end()
{
   # Scans qstat and waits for verification cases to end
   while [[ `qstat -a | grep $(whoami) | grep $JOBPREFIX` != '' ]]; do
      JOBS_REMAINING=`qstat -a | grep $(whoami) | grep $JOBPREFIX | wc -l`
      echo "Waiting for ${JOBS_REMAINING} verification cases to complete." >> $CFASTBOT_DIR/output/stage5
      TIME_LIMIT_STAGE="5"
      check_time_limit
      sleep 60
   done
}

run_verification_cases_long()
{
   # Start running all CFAST verification cases (run all cases on firebot queue)
   cd $CFAST_SVNROOT/Validation/scripts
   echo 'Running CFAST verification cases:' >> $CFASTBOT_DIR/output/stage5 2>&1
   ./Run_CFAST_Cases.sh -q $CFASTBOT_QUEUE >> $CFASTBOT_DIR/output/stage5 2>&1

   # Wait for all verification cases to end
   wait_verification_cases_long_end
}

check_verification_cases_long()
{
   # Scan and report any errors in FDS verification cases
   cd $CFAST_SVNROOT/Validation/

   if [[ `grep 'Run aborted' -rI ${CFASTBOT_DIR}/output/stage5` == "" ]] && \
      [[ `grep ERROR: -rI *` == "" ]] && \
      [[ `grep 'STOP: Numerical' -rI *` == "" ]] && \
      [[ `grep -A 20 forrtl -rI *` == "" ]]
   then
      stage5_success=true
   else
      grep 'Run aborted' -rI $CFASTBOT_DIR/output/stage5 > $CFASTBOT_DIR/output/stage5_errors
      grep ERROR: -rI * >> $CFASTBOT_DIR/output/stage5_errors
      grep 'STOP: Numerical' -rI * >> $CFASTBOT_DIR/output/stage5_errors
      grep -A 20 forrtl -rI * >> $CFASTBOT_DIR/output/stage5_errors
      
      echo "Errors from Stage 5 - Run verification cases (long run):" >> $ERROR_LOG
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
   # smokezip:
   cd $FDS_SVNROOT/Utilities/smokezip/intel_linux_64
   rm -f *.o smokezip_linux_64
   echo 'Compiling smokezip:' > $CFASTBOT_DIR/output/stage6a 2>&1
   ./make_zip.sh >> $CFASTBOT_DIR/output/stage6a 2>&1
   echo "" >> $CFASTBOT_DIR/output/stage6a 2>&1
   
   # smokediff:
   cd $FDS_SVNROOT/Utilities/smokediff/intel_linux_64
   rm -f *.o smokediff_linux_64
   echo 'Compiling smokediff:' >> $CFASTBOT_DIR/output/stage6a 2>&1
   ./make_diff.sh >> $CFASTBOT_DIR/output/stage6a 2>&1
   echo "" >> $CFASTBOT_DIR/output/stage6a 2>&1
   
   # background:
   cd $FDS_SVNROOT/Utilities/background/intel_linux_32
   rm -f *.o background
   echo 'Compiling background:' >> $CFASTBOT_DIR/output/stage6a 2>&1
   ./make_background.sh >> $CFASTBOT_DIR/output/stage6a 2>&1
}

check_smv_utilities()
{
   # Check for errors in SMV utilities compilation
   cd $FDS_SVNROOT
   if [ -e "$FDS_SVNROOT/Utilities/smokezip/intel_linux_64/smokezip_linux_64" ]  && \
      [ -e "$FDS_SVNROOT/Utilities/smokediff/intel_linux_64/smokediff_linux_64" ]  && \
      [ -e "$FDS_SVNROOT/Utilities/background/intel_linux_32/background" ]
   then
      stage6a_success=true
   else
      echo "Errors from Stage 6a - Compile SMV utilities:" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage6a >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#  ==================================
#  = Stage 6b - Compile SMV DB =
#  ==================================

compile_smv_db()
{
   # Clean and compile SMV DB
   cd $FDS_SVNROOT/SMV/Build/intel_linux_64_db
   rm -f smokeview_linux_64_db
   ./make_smv.sh &> $CFASTBOT_DIR/output/stage6b
}

check_compile_smv_db()
{
   # Check for errors in SMV DB compilation
   cd $FDS_SVNROOT/SMV/Build/intel_linux_64_db
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

#  ==================================================
#  = Stage 6c - Make SMV pictures (debug mode) =
#  ==================================================

make_cfast_pictures_db()
{
   # Run Make SMV Pictures script (debug mode)
   cd $FDS_SVNROOT/Verification/scripts
   ./Make_SMV_Pictures.sh -d 2>&1 | grep -v FreeFontPath &> $CFASTBOT_DIR/output/stage6c

}

check_cfast_pictures_db()
{
   # Scan and report any errors in make SMV pictures process
   cd $CFASTBOT_DIR
   if [[ `grep -B 50 -A 50 "Segmentation" -I $CFASTBOT_DIR/output/stage6c` == "" && `grep "*** Error" -I $CFASTBOT_DIR/output/stage6c` == "" ]]
   then
      stage6c_success=true
   else
      cp $CFASTBOT_DIR/output/stage6c $CFASTBOT_DIR/output/stage6c_errors

      echo "Errors from Stage 6c - Make SMV pictures (debug mode):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage6c_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#  ==================================
#  = Stage 6d - Compile SMV release =
#  ==================================

compile_smv()
{
   # Clean and compile SMV
   cd $FDS_SVNROOT/SMV/Build/intel_linux_64
   rm -f smokeview_linux_64
   ./make_smv.sh &> $CFASTBOT_DIR/output/stage6d
}

check_compile_smv()
{
   # Check for errors in SMV release compilation
   cd $FDS_SVNROOT/SMV/Build/intel_linux_64
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

make_cfast_pictures()
{
   # Run Make SMV Pictures script (release mode)
   cd $CFAST_SVNROOT/Validatio/scripts
   ./Make_CFAST_Pictures.sh 2>&1 | grep -v FreeFontPath &> $CFASTBOT_DIR/output/stage6e
}

check__pictures()
{
   # Scan and report any errors in make SMV pictures process
   cd $CFASTBOT_DIR
   if [[ `grep -B 50 -A 50 "Segmentation" -I $CFASTBOT_DIR/output/stage6e` == "" && `grep "*** Error" -I $CFASTBOT_DIR/output/stage6e` == "" ]]
   then
      stage6e_success=true
   else
      cp $CFASTBOT_DIR/output/stage6e  $CFASTBOT_DIR/output/stage6e_errors

      echo "Errors from Stage 6e - Make CFAST pictures (release mode):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage6e >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#  ====================
#  = Stage 7 - Matlab =
#  ====================

# Functions to check for an available Matlab license

run_matlab_license_test()
{
   # Run simple test to see if Matlab license is available
   cd $CFAST_SVNROOT/Utilities/Matlab
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

#  ===========================================
#  = Stage 7b - Matlab plotting (validation) =
#  ===========================================

run_matlab_validation()
{
   # Run Matlab plotting script
   cd $CFAST_SVNROOT/Utilities/Matlab/scripts

   # Replace LaTeX with TeX for Interpreter in plot_style.m
   # This allows displayless automatic Matlab plotting
   # Otherwise Matlab crashes due to a known bug
   sed -i 's/LaTeX/TeX/g' plot_style.m
   
   cd $CFAST_SVNROOT/Utilities/Matlab
   matlab -r "try, disp('Running Matlab Validation script'), CFAST_validation_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $CFASTBOT_DIR/output/stage7b_validation

   matlab -r "try, disp('Running Matlab Verification script'), CFAST_verification_script, catch, disp('Error'), err = lasterror, err.message, err.stack, end, exit" &> $CFASTBOT_DIR/output/stage7c_validation

   # Restore LaTeX as plot_style interpreter
   cd $CFAST_SVNROOT/Utilities/Matlab/scripts
   sed -i 's/TeX/LaTeX/g' plot_style.m
   cd ..
}

check_matlab_validation()
{
   # Scan and report any errors in Matlab scripts
   cd $CFASTBOT_DIR
   if [[ `grep -A 50 "Error" $CFASTBOT_DIR/output/stage7b_validation` == "" ]]
   then
      stage7b_success=true
   else
      grep -A 50 "Error" $CFASTBOT_DIR/output/stage7b_validation > $CFASTBOT_DIR/output/stage7b_errors

      echo "Errors from Stage 7b - Matlab plotting (validation):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage7b_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
   if [[ `grep -A 50 "Error" $CFASTBOT_DIR/output/stage7c_validation` == "" ]]
   then
      stage7c_success=true
   else
      grep -A 50 "Error" $CFASTBOT_DIR/output/stage7c_validation > $CFASTBOT_DIR/output/stage7c_errors

      echo "Errors from Stage 7c - Matlab plotting (validation):" >> $ERROR_LOG
      cat $CFASTBOT_DIR/output/stage7c_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#  ==================================
#  = Stage 8 - Build CFAST Guides =
#  ==================================

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
      # Copy guide to Firebot's local website
      cp $2 /var/www/html/cfastbot/manuals/
   fi
}

make_cfast_tech_guide()
{
   # Build CFAST tech Guide
   cd $CFAST_SVNROOT/Docs/Tech_Ref
   ./make_guide.sh &> $CFASTBOT_DIR/output/stage8_cfast_tech_guide

   # Check guide for completion and copy to website if successful
   check_guide $CFASTBOT_DIR/output/stage8_cfast_tech_guide $CFAST_SVNROOT/Docs/Tech_Ref/Tech_Ref.pdf 'CFAST Technical Reference Guide'
}

make_cfast_vv_guide()
{
   # Build CFAST tech Guide
   cd $CFAST_SVNROOT/Docs/Validation_Guide
   ./make_guide.sh &> $CFASTBOT_DIR/output/stage8_cfast_vv_guide

   # Check guide for completion and copy to website if successful
   check_guide $CFASTBOT_DIR/output/stage8_cfast_vv_guide $CFAST_SVNROOT/Docs/Validation_Guide/Validation_Guide.pdf 'CFAST Verification and Validation Guide'
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
     echo "Build failure and warnings for Revision ${SVN_REVISION}." > "$CFASTBOT_DIR/history/${SVN_REVISION}.txt"
     cat $ERROR_LOG > "$CFASTBOT_DIR/history/${SVN_REVISION}_errors.txt"
     touch output/status_errors_and_warnings

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
      echo "Build failure for Revision ${SVN_REVISION}." > "$CFASTBOT_DIR/history/${SVN_REVISION}.txt"
      cat $ERROR_LOG > "$CFASTBOT_DIR/history/${SVN_REVISION}_errors.txt"
      touch output/status_errors

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
      echo "Revision ${SVN_REVISION} has warnings." > "$CFASTBOT_DIR/history/${SVN_REVISION}.txt"
      cat $WARNING_LOG > "$CFASTBOT_DIR/history/${SVN_REVISION}_warnings.txt"
      touch output/status_warnings

   # No errors or warnings
   else
      echo "Build success! Revision ${SVN_REVISION} passed all build tests." > "$CFASTBOT_DIR/history/${SVN_REVISION}.txt"
      touch output/status_success
   fi
}

email_build_status()
{
   echo $THIS_CFAST_FAILED>$CFAST_STATUS_FILE
   stop_time=`date`
   echo "-------------------------------" > $TIME_LOG
   echo "Host: $hostname " >> $TIME_LOG
   echo "Start Time: $start_time " >> $TIME_LOG
   echo "Stop Time: $stop_time " >> $TIME_LOG
   echo "-------------------------------" > $TIME_LOG
   echo "Nightly Manuals (public): https://drive.google.com/folderview?id=0B_wB1pJL2bFQSkhyNDJ0bEw0cVE#list" >> $TIME_LOG
   if [[ $THIS_SMVSVN != $LAST_SMVSVN ]] ; then
     cat $SVN_SMVLOG >> $TIME_LOG
   fi
   if [[ $THIS_CFASTSOURCESVN != $LAST_CFASTSOUCESVN ]] ; then
     cat $SVN_CFASTSOURCELOG >> $TIME_LOG
   fi
   if [[ $THIS_CFASTDOCSSVN != $LAST_CFASTDOCSSVN ]] ; then
     cat $SVN_CFASTDOCSLOG >> $TIME_LOG
   fi
   cd $CFASTBOT_DIR
   # Check for warnings and errors
   if [[ -e $WARNING_LOG && -e $ERROR_LOG ]]
   then
     cat $TIME_LOG >> $ERROR_LOG
     cat $TIME_LOG >> $WARNING_LOG
     # Send email with failure message and warnings, body of email contains appropriate log file
     mail -s "cfastbot build failure and warnings on ${hostname}. Revision ${SVN_REVISION}." $mailTo < $ERROR_LOG > /dev/null

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
     cat $TIME_LOG >> $ERROR_LOG
      # Send email with failure message, body of email contains error log file
      mail -s "cfastbot build failure on ${hostname}. Revision ${SVN_REVISION}." $mailTo < $ERROR_LOG > /dev/null

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
     cat $TIME_LOG >> $WARNING_LOG
      # Send email with success message, include warnings
      mail -s "cfastbot build success with warnings on ${hostname}. Revision ${SVN_REVISION}." $mailTo < $WARNING_LOG > /dev/null

   # No errors or warnings
   else
      # Send empty email with success message
      mail -s "cfastbot build success on ${hostname}! Revision ${SVN_REVISION}." $mailTo < $TIME_LOG > /dev/null
   fi
}

# if -a option is invoked, only proceed running cfastbot if the
# smokeview or FDS source has changed

if [[ $RUNAUTO == "y" ]] ; then
  run_auto
fi

#  ============================
#  = Primary script execution =
#  ============================

hostname=`hostname`
start_time=`date`
clean_cfastbot_history

### Stage 0 ###
update_and_compile_cfast

### Stage 1 ###
clean_svn_repo
do_svn_checkout
check_svn_checkout
#if [[ ! $SKIP_SVN_PROPS ]] ; then
   #fix_svn_properties
#fi

### Stage 3 ###
if [[ $stage0_success ]] ; then
   run_verification_cases_short
   check_verification_cases_short
fi

### Stage 5 ###
if [[ $stage0_success ]] ; then
   run_verification_cases_long
   check_verification_cases_long
fi

### Stage 6a ###
# No stage dependencies
compile_smv_utilities
check_smv_utilities

### Stage 6b ###
# No stage dependencies
compile_smv_db
check_compile_smv_db

### Stage 6c ###
if [[ $stage4a_success && $stage6b_success ]] ; then
dummy=
#  make_cfast_pictures_db
#  check_cfast_pictures_db
fi

### Stage 6d ###
compile_smv
check_compile_smv

### Stage 6e ###
if [[ $stage4a_success && $stage6d_success ]] ; then
dummy=
#  make_cfast_pictures
#  check_cfast_pictures
fi

### Stage 7b ###
check_matlab_license_server
run_matlab_validation
check_matlab_validation


### Stage 8 ###
# No stage dependencies
  make_cfast_tech_guide
  make_cfast_vv_guide

### Report results ###
set_files_world_readable
save_build_status
email_build_status

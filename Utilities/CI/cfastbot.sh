#!/bin/bash
set -o pipefail
CI_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
source "$CI_DIR/wait_cases.sh"
source "$CI_DIR/setup_python.sh"
cur_dir=$CI_DIR
# CFASTbot
# This script runs the CFAST verification/validation suite

run_logged()
{
   local log=$1
   shift
   "$@" > "$log" 2>&1
   local result=$?
   if [[ $result != 0 ]]; then
      echo "Command failed (exit $result): $*" >> "$ERROR_LOG"
      cat "$log" >> "$ERROR_LOG"
   fi
   return "$result"
}

#---------------------------------------------
#                   run_auto
#---------------------------------------------

run_auto()
{
   if cmp -s "$OUTPUT_DIR/revisions.tsv" "$GITSTATUS_DIR/last_successful_revisions.tsv"; then
      echo "Repository revisions are unchanged since the last successful run."
      exit 0
   fi
}

#---------------------------------------------
#                   check_time_limit
#---------------------------------------------

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
         echo -e "CFASTbot has been running for more than 3 hours in Stage ${TIME_LIMIT_STAGE}. \n\nPlease ensure that there are no problems. \n\nThis is a notification only and does not terminate CFASTbot." | mail $REPLYTO -s "CFASTbot Notice: CFASTbot has been running for more than 3 hours." $mailTo &> /dev/null
         TIME_LIMIT_EMAIL_NOTIFICATION="sent"
      fi
   fi
}

#---------------------------------------------
#                   set_files_world_readable
#---------------------------------------------

set_files_world_readable()
{
   cd "$cfastrepo" || exit 1
   chmod -R go+r *

   cd "$smvrepo" || exit 1
   chmod -R go+r *

   cd "$exprepo" || exit 1
   chmod -R go+r *

   return 0
}

#---------------------------------------------
#                   setup_python_environment
#---------------------------------------------

setup_python_environment()
{
   local setup_log="$1"

   if [ "$PYTHON_ENV_ACTIVE" == "1" ]; then
      echo "Python environment already active." > "$setup_log"
      return 0
   fi

   setup_cfast_python "$reporoot" > "$setup_log" 2>&1 || {
      cat "$setup_log" >> "$ERROR_LOG"
      return 1
   }
   PYTHON_ENV_ACTIVE=1
   return 0
}

#---------------------------------------------
#                   check_compile_cfast_db
#---------------------------------------------

check_compile_cfast_db()
{
   # Check for errors in CFAST debug compilation
   cd "$cfastrepo/Build/CFAST/${compiler}_${cfast_platform}_db" || exit 1
   if [ -e "cfast8_${cfast_platform}_db" ]
   then
      stage2_build_cfast_debug_success=true
   else
      echo "Errors from Stage 2 - Compile CFAST debug:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage2_build_cfast_debug >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_cfast_debug` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 2 - Compile CFAST debug:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_cfast_debug >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#---------------------------------------------
#                   check_compile_cfast
#---------------------------------------------

check_compile_cfast()
{
   # Check for errors in CFAST release compilation
   cd "$cfastrepo/Build/CFAST/${compiler}_${cfast_platform}" || exit 1
   if [[ -e "cfast8_${cfast_platform}" ]]
   then
      stage2_build_cfast_release_success=true
   else
      echo "Errors from Stage 2 - Compile CFAST release:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage2_build_cfast_release >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_cfast_release` == "" ]]
   then
      # Continue along
      :
   else
      echo "Warnings from Stage 2 - Compile CFAST release:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_cfast_release >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
   return 0
}

#---------------------------------------------
#                   check_compile_smv_db
#---------------------------------------------

check_compile_smv_db()
{
   # Check for errors in SMV DB compilation
   cd "$smvrepo/Build/smokeview/intel_${platform}" || exit 1
   if [ -e "smokeview_${platform}_db" ]
   then
      stage2_build_smv_debug_success=true
   else
      echo "Errors from Stage 2 - Compile SMV DB:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage2_build_smv_debug >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_smv_debug | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
   then
      # Continue along
      :
   else
      echo "Stage build_smv_debug warnings:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_smv_debug | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
   fi
   return 0
}

#---------------------------------------------
#                   check_compile_smv
#---------------------------------------------

check_compile_smv()
{
   # Check for errors in SMV release compilation
   cd "$smvrepo/Build/smokeview/intel_${platform}" || exit 1
   if [ -e "smokeview_${platform}" ]
   then
      stage2_build_smv_release_success=true
   else
      echo smokeview not found
      echo "Errors from Stage 2 - Compile SMV release:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage2_build_smv_release >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi

   # Check for compiler warnings/remarks
   # grep -v 'feupdateenv ...' ignores a known FDS MPI compiler warning (http://software.intel.com/en-us/forums/showthread.php?t=62806)
   if [[ `grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_smv_release | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked'` == "" ]]
   then
      # Continue along
      :
   else
      echo "Stage build_smv_release warnings:" >> $WARNING_LOG
      grep -A 5 -E 'warning|remark' ${OUTPUT_DIR}/stage2_build_smv_release | grep -v 'feupdateenv is not implemented' | grep -v 'lcilkrts linked' >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
   return 0
}

#---------------------------------------------
#                   vv_jobs_remaining
#---------------------------------------------

submit_suite()
{
   local suite=$1 run_log=$2
   shift 2
   local result=0
   export CFAST_JOB_MANIFEST="$run_log.jobs"
   : > "$CFAST_JOB_MANIFEST"
   "$CI_DIR/Run_CFAST_Cases.sh" --repo-root "$cfastrepo" --suite "$suite" "$@" > "$run_log" 2>&1 || result=1
   TIME_LIMIT_STAGE="3 $suite cases"
   wait_case_manifest "$CFAST_JOB_MANIFEST" >> "$run_log" 2>&1 || result=1
   if [[ $result != 0 ]]; then
      cat "$run_log" >> "$ERROR_LOG"
   fi
   return "$result"
}

#---------------------------------------------
#                   run_vv_cases_debug
#---------------------------------------------

run_vv_cases_debug()
{
   local verification_log="$OUTPUT_DIR/stage3_run_debug_verification"
   local validation_log="$OUTPUT_DIR/stage3_run_debug_validation"

   #  =======================
   #  = Run all cfast cases =
   #  =======================

   # Submit CFAST V&V cases
   echo 'Running CFAST V&V cases'
   echo '   debug'
   echo 'Running CFAST V&V cases' >> $OUTPUT_DIR/stage3_run_debug 2>&1

   submit_suite Verification "$verification_log" -I "$compiler" -S "$smvrepo" -m 2 -d -j "$JOBPREFIX" -q "$QUEUE" || return 1
   cat "$verification_log" >> $OUTPUT_DIR/stage3_run_debug 2>&1

   submit_suite Validation "$validation_log" -I "$compiler" -S "$smvrepo" -m 2 -d -j "$JOBPREFIX" -q "$QUEUE" || return 1
   cat "$validation_log" >> $OUTPUT_DIR/stage3_run_debug 2>&1
   return 0
}

#---------------------------------------------
#                   check_vv_cases_debug
#---------------------------------------------

check_vv_cases_debug()
{
   # Scan and report any errors in CFAST Verification cases
   cd "$cfastrepo/Verification" || exit 1

   if [[ `grep 'Run aborted' -riI --include "*.log" --include "*.err" ${OUTPUT_DIR}/stage3_run_debug` == "" ]] && \
      [[ `grep -F "***Error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include "*.log" --include "*.err" *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include "*.log" --include "*.err" $OUTPUT_DIR/stage3_run_debug >> $OUTPUT_DIR/stage3_run_debug_errors
      grep -F "***Error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_debug_errors
      grep -F "***Fatal error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_debug_errors
      grep -A 20 forrtl -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_debug_errors

      echo "Errors from Stage 3 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage3_run_debug_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd "$cfastrepo/Validation" || exit 1

   if [[ `grep 'Run aborted' -riI --include "*.log" --include "*.err" ${OUTPUT_DIR}/stage3_run_debug` == "" ]] && \
      [[ `grep -F "***Error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include "*.log" --include "*.err" *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include "*.log" --include "*.err" $OUTPUT_DIR/stage3_run_debug >> $OUTPUT_DIR/stage3_run_debug_errors
      grep -F "***Error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_debug_errors
      grep -F "***Fatal error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_debug_errors
      grep -A 20 forrtl -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_debug_errors

      echo "Errors from Stage 3 - Run V&V cases (debug mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage3_run_debug_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   return 0
}

#---------------------------------------------
#                   run_vv_cases_release
#---------------------------------------------

run_vv_cases_release()
{
   local verification_log="$OUTPUT_DIR/stage3_run_release_verification"
   local validation_log="$OUTPUT_DIR/stage3_run_release_validation"

   # Start running all CFAST V&V cases
   echo '   release'
   echo 'Running CFAST V&V cases' >> $OUTPUT_DIR/stage3_run_release 2>&1

   submit_suite Verification "$verification_log" -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" -q "$QUEUE" || return 1
   cat "$verification_log" >> $OUTPUT_DIR/stage3_run_release 2>&1

   submit_suite Validation "$validation_log" -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" -q "$QUEUE" || return 1
   cat "$validation_log" >> $OUTPUT_DIR/stage3_run_release 2>&1
   return 0
}

#---------------------------------------------
#                   check_vv_cases_release
#---------------------------------------------

check_vv_cases_release()
{
   # Scan and report any errors in CFAST Verificaion cases
   cd "$cfastrepo/Verification" || exit 1

   if [[ `grep 'Run aborted' -riI --include "*.log" --include "*.err" ${OUTPUT_DIR}/stage3_run_release` == "" ]] && \
      [[ `grep -F "***Error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include "*.log" --include "*.err" *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include "*.log" --include "*.err" $OUTPUT_DIR/stage3_run_release >> $OUTPUT_DIR/stage3_run_release_errors
      grep -F "***Error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_release_errors
      grep -F "***Fatal error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_release_errors
      grep -A 20 forrtl -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_release_errors

      echo "Errors from Stage 3 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage3_run_release_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi

   # Scan and report any errors in CFAST Validation cases
   cd "$cfastrepo/Validation" || exit 1

   if [[ `grep 'Run aborted' -riI --include "*.log" --include "*.err" ${OUTPUT_DIR}/stage3_run_release` == "" ]] && \
      [[ `grep -F "***Error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -F "***Fatal error" -riI --include "*.log" --include "*.err" *` == "" ]] && \
      [[ `grep -A 20 forrtl -riI --include "*.log" --include "*.err" *` == "" ]]
   then
      :
   else
      grep 'Run aborted' -riI --include "*.log" --include "*.err" $OUTPUT_DIR/stage3_run_release >> $OUTPUT_DIR/stage3_run_release_errors
      grep -F "***Error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_release_errors
      grep -F "***Fatal error" -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_release_errors
      grep -A 20 forrtl -riI --include "*.log" --include "*.err" * >> $OUTPUT_DIR/stage3_run_release_errors

      echo "Errors from Stage 3 - Run V&V cases (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage3_run_release_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi
   return 0
}

#---------------------------------------------
#                   run_ceditqt_cases_release
#---------------------------------------------

run_ceditqt_cases_release()
{
   local verification_log="$OUTPUT_DIR/stage3_run_release_ceditqt_verification"
   local validation_log="$OUTPUT_DIR/stage3_run_release_ceditqt_validation"

   echo '   CEditQt UI'
   echo 'Running CEditQt V&V UI import/rewrite tests' > $OUTPUT_DIR/stage3_run_release_ceditqt 2>&1

   setup_python_environment $OUTPUT_DIR/stage3_ceditqt_python_setup || return 1

   submit_suite Verification "$verification_log" --test-UI -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" -q "$QUEUE" || return 1
   cat "$verification_log" >> $OUTPUT_DIR/stage3_run_release_ceditqt 2>&1

   submit_suite Validation "$validation_log" --test-UI -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" -q "$QUEUE" || return 1
   cat "$validation_log" >> $OUTPUT_DIR/stage3_run_release_ceditqt 2>&1
   return 0
}

#---------------------------------------------
#                   check_ceditqt_cases_release
#---------------------------------------------

check_ceditqt_cases_release()
{
   : > $OUTPUT_DIR/stage3_run_release_ceditqt_errors

   cd "$cfastrepo/Verification" || exit 1
   grep 'Run aborted' -riI --include "*.ui.log" --include "*.ui.err" * >> $OUTPUT_DIR/stage3_run_release_ceditqt_errors
   grep -E "FAIL|Traceback|SyntaxError|ImportError|ModuleNotFoundError|RuntimeError" -riI --include "*.ui.log" --include "*.ui.err" * >> $OUTPUT_DIR/stage3_run_release_ceditqt_errors

   cd "$cfastrepo/Validation" || exit 1
   grep 'Run aborted' -riI --include "*.ui.log" --include "*.ui.err" * >> $OUTPUT_DIR/stage3_run_release_ceditqt_errors
   grep -E "FAIL|Traceback|SyntaxError|ImportError|ModuleNotFoundError|RuntimeError" -riI --include "*.ui.log" --include "*.ui.err" * >> $OUTPUT_DIR/stage3_run_release_ceditqt_errors

   if [ -s $OUTPUT_DIR/stage3_run_release_ceditqt_errors ]; then
      echo "Errors from Stage 3 - CEditQt V&V UI import/rewrite tests:" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage3_run_release_ceditqt_errors >> $ERROR_LOG
      echo "" >> $ERROR_LOG
      THIS_CFAST_FAILED=1
   fi
   return 0
}

#---------------------------------------------
#                   check_cfast_pictures
#---------------------------------------------

check_cfast_pictures()
{
   # Scan and report any errors in make SMV pictures process
   cd "$cfastbotdir" || exit 1
   if [[ `grep -B 10 -A 10 "Segmentation" -I $OUTPUT_DIR/stage4_make_pictures` == "" && `grep -F "*** Error" -I $OUTPUT_DIR/stage4_make_pictures` == "" ]]
   then
      stage4_make_pictures_success=true
   else
      cp $OUTPUT_DIR/stage4_make_pictures  $OUTPUT_DIR/stage4_make_pictures_errors
      echo "Errors from Stage 4 - Make CFAST pictures (release mode):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage4_make_pictures >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#---------------------------------------------
#                   check_python_verification
#---------------------------------------------

check_python_verification()
{
   # Scan and report any errors in Python scripts
   cd "$cfastbotdir" || exit 1

   if [[ `grep -A 50 "Error" $OUTPUT_DIR/stage5_run_python_verification` == "" ]]
   then
      stage5_run_python_verification_success=true
   else
      grep -A 50 "Error" $OUTPUT_DIR/stage5_run_python_verification >> $OUTPUT_DIR/stage5_run_python_verification_errors

      echo "Warnings from Stage 5 - Python plotting (verification):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage5_run_python_verification_errors | tr -cd '\11\12\15\40-\176' >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#---------------------------------------------
#                   check_python_validation
#---------------------------------------------

check_python_validation()
{
   # Scan and report any errors in Python scripts
   cd "$cfastbotdir" || exit 1
   if [[ `grep -A 50 "Error" $OUTPUT_DIR/stage5_run_python_validation` == "" ]]
   then
      stage5_run_python_validation_success=true
   else
      grep -A 50 "Error" $OUTPUT_DIR/stage5_run_python_validation >> $OUTPUT_DIR/stage5_run_python_validation_errors

      echo "Errors from Stage 5 - Python plotting and statistics (validation):" >> $ERROR_LOG
      cat $OUTPUT_DIR/stage5_run_python_validation_errors |  tr -cd '\11\12\15\40-\176' >> $ERROR_LOG
      echo "" >> $ERROR_LOG
   fi
}

#---------------------------------------------
#                   check_validation_stats
#---------------------------------------------

check_validation_stats()
{
   cd "$cfastrepo/Utilities/Python" || exit 1

   STATS_FILE_BASENAME=validation_scatterplot_output

   BASELINE_STATS_FILE=$cfastrepo/Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES/Scatterplots/${STATS_FILE_BASENAME}_baseline.csv
   CURRENT_STATS_FILE=$cfastrepo/Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES/Scatterplots/${STATS_FILE_BASENAME}.csv

   if [ -e ${CURRENT_STATS_FILE} ]
   then
      if [[ `diff -u <(sed 's/"//g' ${BASELINE_STATS_FILE}) <(sed 's/"//g' ${CURRENT_STATS_FILE})` == "" ]]
      then
         # Continue along
         :
      else
         echo "Warnings from stage 5 - Python plotting and statistics (validation):" >> $VALIDATION_STATS_LOG
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
      echo "Warnings from stage 5 - Python plotting and statistics (validation):" >> $WARNING_LOG
      echo "Error: The validation statistics output file does not exist." >> $WARNING_LOG
      echo "Expected the file /Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES/Scatterplots/validation_scatterplot_output.csv" >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
   return 0
}

#---------------------------------------------
#                   archive_validation_stats
#---------------------------------------------

archive_validation_stats()
{
   cd "$cfastrepo/Utilities/Python" || exit 1

   if [ -e ${CURRENT_STATS_FILE} ] ; then
      # Copy to CFASTbot history
      cp ${CURRENT_STATS_FILE} "$HISTORY_DIR/${STATS_FILE_BASENAME}_${GIT_REVISION}.csv"
   fi
   cd "$cfastrepo/Validation/scripts" || exit 1
   if [ -e gettime.sh ]; then
     TIMEFILE=$HISTORY_DIR/${GIT_REVISION}_timing.csv
     "$CI_DIR/Run_CFAST_Cases.sh" --repo-root "$cfastrepo" --suite Validation -t > "$TIMEFILE"
     total_time=`cat $TIMEFILE | awk -F',' '{ SUM += $2} END { print SUM }'`
     echo $total_time >> $TIMEFILE
   fi
   return 0
}

#---------------------------------------------
#                   check_guide
#---------------------------------------------

check_guide()
{
   local logfile=$1
   local docdir=$2
   local docfile=$3
   local docname=$4

   # Scan and report any errors or warnings in build process for guides
   cd "$cfastbotdir" || exit 1
   if [[ `grep -I "succeeded" $logfile` != "" ]] && [[ -e $docdir/$docfile ]]; then
      # Guide built succeeded; there were no errors/warnings
      # Copy guide to CFASTbot's local website
      dummy=1
   else
      # There were errors/warnings in the guide build process
      echo "Warnings from Stage 6 - Build CFAST Guides:" >> $WARNING_LOG
      echo $docname >> $WARNING_LOG # Name of guide
      if [ ! -e $docdir/$docfile ]; then
         echo The guide $docname failed to be built >> $WARNING_LOG
         echo "" >> $WARNING_LOG
      fi
      cat $logfile >> $WARNING_LOG
      echo "" >> $WARNING_LOG
   fi
}

#---------------------------------------------
#                   upload_linux_bundle
#---------------------------------------------

upload_linux_bundle()
{
   local bundle_script="$cfastrepo/Build/bundle/build_linux_bundle.sh"
   local cedit_script="$cfastrepo/Build/CeditQt/build_linux_app.sh"
   local bundle_log="$OUTPUT_DIR/stage7_upload_linux_bundle"

   if [[ "$UPLOAD" != "1" || "$platform" != "linux" ]]; then
      return 0
   fi
   if [[ -e $ERROR_LOG || -e $WARNING_LOG ]]; then
      return 0
   fi

   echo "Building and uploading CFAST Linux bundle"
   # Package the binaries and manuals from this run without changing revisions.
   if "$cedit_script" --python "$CFAST_PYTHON" > "$bundle_log" 2>&1 &&
      "$bundle_script" --no-update-repos --no-build-cfast --no-build-smokeview \
         --no-build-manuals --no-upload-manuals \
         --cfast-exe "$cfastrepo/Build/CFAST/${compiler}_linux/cfast8_linux" \
         --smokeview-exe "$smvrepo/Build/smokeview/intel_linux/smokeview_linux" \
         --output-dir "$OUTPUT_DIR/bundles" --stage-dir "$OUTPUT_DIR/bundle_stage" \
         --upload --upload-release-repo "$GH_OWNER/$GH_REPO" \
         --upload-release-tag "$GH_CFAST_TAG" >> "$bundle_log" 2>&1; then
      return 0
   fi
   echo "Errors from Stage 7 - Build/upload CFAST Linux bundle:" >> "$ERROR_LOG"
   cat "$bundle_log" >> "$ERROR_LOG"
   echo "" >> "$ERROR_LOG"
   return 1
}

#---------------------------------------------
#                   save_build_status
#---------------------------------------------

save_build_status()
{
   cd "$cfastbotdir" || exit 1
   # Save status outcome of build to a text file
   if [[ -e $WARNING_LOG && -e $ERROR_LOG ]]
   then
     echo "" >> $ERROR_LOG
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

#---------------------------------------------
#                   email_build_status
#---------------------------------------------

email_build_status()
{
   echo $THIS_CFAST_FAILED>$CFAST_STATUS_FILE
   stop_time=`date`
   IFORT_VERSION=`ifx -v 2>&1`
   if [[ $SKIP_git_UPDATE_AND_PROPFIX ]] ; then
      echo "CFASTbot was invoked with the -s option (SKIP_git_UPDATE_AND_PROPFIX)." >> $TIME_LOG
      echo "Skipping git revert, update, and property fix operations." >> $TIME_LOG
      echo "The current git revision is ${GIT_REVISION}" >> $TIME_LOG
   fi
   echo ""                                     >> $TIME_LOG
   echo "Host: $hostname "                     >> $TIME_LOG
   echo "repo: $cfastrepo "                    >> $TIME_LOG
   echo "Fortran: $IFORT_VERSION "             >> $TIME_LOG
   echo ""                                     >> $TIME_LOG
   echo "$BOT_REVISION "                       >> $TIME_LOG
   echo "$CFAST_REVISION "                     >> $TIME_LOG
   echo "$EXP_REVISION "                       >> $TIME_LOG
   echo "$FDS_REVISION "                       >> $TIME_LOG
   echo "$SMV_REVISION "                       >> $TIME_LOG
   echo ""                                     >> $TIME_LOG
   echo "Start Time: $start_time "             >> $TIME_LOG
   echo "Stop Time: $stop_time "               >> $TIME_LOG
   if [ "$total_time" != "" ]; then
     echo "Run Time: $total_time"              >> $TIME_LOG
   fi
   cd "$cfastbotdir" || exit 1
   # Check for warnings and errors
   if [[ -e $WARNING_LOG && -e $ERROR_LOG ]]
   then
     cat $TIME_LOG >> $WARNING_LOG
     # Send email with failure message and warnings, body of email contains appropriate log file
     cat $ERROR_LOG $TIME_LOG | mail $REPLYTO -s "CFASTbot build failure and warnings on ${hostname}. Revision ${GIT_REVISION}." $mailTo &> /dev/null

   # Check for errors only
   elif [ -e $ERROR_LOG ]
   then
      # Send email with failure message, body of email contains error log file
      cat $ERROR_LOG $TIME_LOG | mail $REPLYTO -s "CFASTbot build failure on ${hostname}. Revision ${GIT_REVISION}." $mailTo &> /dev/null

   # Check for warnings only
   elif [ -e $WARNING_LOG ]
   then
      # Send email with success message, include warnings
      cat $WARNING_LOG $TIME_LOG | mail $REPLYTO -s "CFASTbot build success with warnings on ${hostname}. Revision ${GIT_REVISION}." $mailTo &> /dev/null

   # No errors or warnings
   else
      if [[ "$UPLOAD" == "1" ]] && [[ -e $GUIDES2GH ]]; then
         cd "$cfastbotdir" || exit 1
         "$GUIDES2GH" "$cfastrepo/Manuals" "$GITSTATUS_DIR/VERSION_LATEST" > "$OUTPUT_DIR/stage7_upload" 2>&1 || { cat "$OUTPUT_DIR/stage7_upload" >> "$ERROR_LOG"; return 1; }
         GITURL=https://github.com/$GH_OWNER/$GH_REPO/releases/tag/$GH_CFAST_TAG
         echo ""                                  >> $TIME_LOG
         echo "Linux bundle, Manuals: $GITURL"    >> $TIME_LOG
      fi
      # Send empty email with success message
      cat $TIME_LOG | mail $REPLYTO -s "CFASTbot build success on ${hostname}! Revision ${GIT_REVISION}." $mailTo &> /dev/null
   fi

   # Send email notification if validation statistics have changed.
   if [ -e $VALIDATION_STATS_LOG ]
   then
      mail $REPLYTO -s "CFASTbot notice. Validation statistics have changed for Revision ${GIT_REVISION}." $mailTo < $VALIDATION_STATS_LOG &> /dev/null
   fi
}

#VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
#                             start of script
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


#  ===================
#  = Input variables =
#  ===================

cfastbotdir=$CI_DIR
reporoot=$(cd "$CI_DIR/../../.." && pwd)
GITSTATUS_DIR=${CFAST_CI_STATE_DIR:-$HOME/.cfastbot}
OUTPUT_DIR=${CFAST_CI_OUTPUT_DIR:?Run this script through run_cfastbot.sh}
HISTORY_DIR=$GITSTATUS_DIR/history
EMAIL_LIST=$GITSTATUS_DIR/cfastbot_email_list.sh
ERROR_LOG=$OUTPUT_DIR/errors
WARNING_LOG=$OUTPUT_DIR/warnings
TIME_LOG=$OUTPUT_DIR/timings
VALIDATION_STATS_LOG=$OUTPUT_DIR/statistics
NEWGUIDE_DIR=$OUTPUT_DIR/NEW_GUIDES
mkdir -p "$HISTORY_DIR"
[[ ! -f $EMAIL_LIST ]] || source "$EMAIL_LIST"
QUEUE=terminal
compiler=intel
RUNAUTO=
UPLOAD=
TEST_UI=
PREFLIGHT=0
PYTHON_ENV_ACTIVE=
while (($#)); do
   case "$1" in
      -r) reporoot=$2; shift 2;;
      -q) QUEUE=$2; shift 2;;
      -I) compiler=$2; shift 2;;
      -m) mailTo=$2; shift 2;;
      -a) RUNAUTO=y; shift;;
      -U) UPLOAD=1; shift;;
      --test-UI) TEST_UI=1; shift;;
      --preflight) PREFLIGHT=1; shift;;
      *) echo "Unknown driver option: $1" >&2; exit 2;;
   esac
done
case "$compiler" in intel|gnu) :;; *) echo 'Compiler must be intel or gnu.' >&2; exit 2;; esac
cfastrepo=$reporoot/cfast
fdsrepo=$reporoot/fds
smvrepo=$reporoot/smv
exprepo=$reporoot/exp
mailTo=${mailTo:-${mailToCFAST:-}}
if [[ -z $mailTo ]]; then mailTo=$(git -C "$cfastrepo" config user.email || whoami); fi
REPLYTO=
for repo in "$cfastrepo" "$fdsrepo" "$smvrepo" "$exprepo"; do
   [[ -d $repo ]] || { echo "Missing repository: $repo" >&2; exit 1; }
done
: > "$OUTPUT_DIR/revisions.tsv"
for name in cfast fds smv exp; do
   printf '%s\t%s\n' "$name" "$(git -C "$reporoot/$name" rev-parse HEAD)" >> "$OUTPUT_DIR/revisions.tsv"
   git -C "$reporoot/$name" submodule status --recursive >> "$OUTPUT_DIR/revisions.tsv"
done
printf 'ci_tooling\t%s\n' "${CFAST_CI_TOOLING_REVISION:-$(git -C "$cfastrepo" rev-parse HEAD)}" >> "$OUTPUT_DIR/revisions.tsv"
if [[ $RUNAUTO == y ]]; then run_auto; fi
missing=0
for program in python3 ifx pdflatex biber; do
   command -v "$program" >/dev/null || { echo "Missing CI prerequisite: $program" >&2; missing=1; }
done
if [[ $compiler == gnu ]]; then command -v gfortran >/dev/null || missing=1; fi
if [[ $QUEUE != terminal && $QUEUE != none ]]; then
   if ! command -v sbatch >/dev/null && ! command -v qsub >/dev/null; then
      echo 'Missing batch scheduler.' >&2; missing=1
   fi
fi
[[ $missing == 0 ]] || exit 1
if [[ $PREFLIGHT == 1 ]]; then echo 'CFAST CI preflight passed.'; exit 0; fi

platform="linux"
if [ "`uname`" == "Darwin" ] ; then
  platform="osx"
fi
cfast_platform=$platform
if [ "$cfast_platform" == "osx" ] ; then
  cfast_platform="macos"
fi
export platform
export cfast_platform

echo "   platform: $platform"
echo "   compiler: $compiler"

# Set unlimited stack size
if [ "$platform" == "linux" ] ; then
  ulimit -s unlimited
fi

if [ "$UPLOAD" == "1" ]; then
  mkdir -p $NEWGUIDE_DIR
fi

cd

THIS_CFAST_FAILED=0
CFAST_STATUS_FILE=$GITSTATUS_DIR/cfast_status
LAST_CFAST_FAILED=0
if [ -e $CFAST_STATUS_FILE ] ; then
   LAST_CFAST_FAILED=`cat $CFAST_STATUS_FILE`
fi

export JOBPREFIX=cb_
GUIDES2GH=$cfastbotdir/guides2GH.sh

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
TIME_LIMIT=14400
TIME_LIMIT_EMAIL_NOTIFICATION="unsent"


hostname=$(hostname)
start_time=$(date)
setup_python_environment "$OUTPUT_DIR/stage0_python_setup" || exit 1

### Stage 1 ###

BOT_REVISION="CI tooling: ${CFAST_CI_TOOLING_REVISION:-$(git -C "$cfastrepo" describe --always --dirty)}"

cd "$reporoot/exp" || exit 1
EXP_REVISION=`git describe --abbrev=7 --dirty --long`

cd "$reporoot/fds" || exit 1
FDS_REVISION=`git describe --abbrev=7 --dirty --long`

cd "$reporoot/cfast" || exit 1
CFAST_REVISION=`git describe --abbrev=7 --dirty --long`
CFAST_SHORTHASH=`git rev-parse --short HEAD`
GIT_REVISION=$CFAST_SHORTHASH
# CFAST_REV same as CFAST_REVISION without the hash on the end
CFAST_REV=`git describe | sed 's/-g[0-9a-f]*$//'`

cd "$reporoot/smv" || exit 1
SMV_REVISION=`git describe --abbrev=7 --dirty --long`
SMV_SHORTHASH=`git rev-parse --short HEAD`
# SMV_REV same as SMV_REVISION without the hash on the end
SMV_REV=`git describe | sed 's/-g[0-9a-f]*$//'`

cd "$cur_dir" || exit 1

### Stage 2 ###

#*** build cfast debug cfast
echo "Building"
echo "   cfast"
echo "      $compiler debug"
cd "$cfastrepo/Build/CFAST/${compiler}_${cfast_platform}_db" || exit 1
./make_cfast.sh --clean-cfast > "$OUTPUT_DIR/stage2_build_cfast_debug" 2>&1 || { cat "$OUTPUT_DIR/stage2_build_cfast_debug" >> "$ERROR_LOG"; exit 1; }
check_compile_cfast_db || exit 1

#*** build release cfast
echo "      release"
cd "$cfastrepo/Build/CFAST/${compiler}_${cfast_platform}" || exit 1
./make_cfast.sh --clean-cfast > "$OUTPUT_DIR/stage2_build_cfast_release" 2>&1 || { cat "$OUTPUT_DIR/stage2_build_cfast_release" >> "$ERROR_LOG"; exit 1; }
check_compile_cfast || exit 1

#*** build smokeview libraries
cd "$smvrepo/Build/LIBS/intel_${platform}" || exit 1
echo 'Building Smokeview libraries' >> $OUTPUT_DIR/stage2_build_smv_util 2>&1
echo "   smokeview libraries"
./make_LIBS.sh > "$OUTPUT_DIR/stage2_build_smv_util" 2>&1 || { cat "$OUTPUT_DIR/stage2_build_smv_util" >> "$ERROR_LOG"; exit 1; }

#*** build debug smokeview
echo "   smokeview"
echo "      debug"
cd "$smvrepo/Build/smokeview/intel_${platform}" || exit 1
./make_smokeview_db.sh > "$OUTPUT_DIR/stage2_build_smv_debug" 2>&1 || { cat "$OUTPUT_DIR/stage2_build_smv_debug" >> "$ERROR_LOG"; exit 1; }
check_compile_smv_db || exit 1

#*** build release smokeview
echo "      release"
cd "$smvrepo/Build/smokeview/intel_${platform}" || exit 1
./make_smokeview.sh > "$OUTPUT_DIR/stage2_build_smv_release" 2>&1 || { cat "$OUTPUT_DIR/stage2_build_smv_release" >> "$ERROR_LOG"; exit 1; }
check_compile_smv || exit 1

### Stage 3 ###

#*** run cases - debug
if [[ $stage2_build_cfast_debug_success ]] ; then
   run_vv_cases_debug || exit 1
   check_vv_cases_debug || exit 1
fi

#*** run cases - release
if [[ $stage2_build_cfast_release_success ]] ; then
   run_vv_cases_release || exit 1
   check_vv_cases_release || exit 1
   if [[ "$TEST_UI" != "" ]]; then
      run_ceditqt_cases_release || exit 1
      check_ceditqt_cases_release || exit 1
   fi
fi

### Stage 4 ###
if [[ $stage2_build_cfast_release_success && $stage2_build_smv_release_success ]] ; then
   echo "Generating smokeview images"
   cd "$cfastrepo/Validation/scripts" || exit 1
   run_logged "$OUTPUT_DIR/stage4_make_pictures" ./Make_CFAST_Pictures.sh -I intel
   check_cfast_pictures
fi

### stage 5 - python verification ###
  echo Python
  echo "   setup environment"
  setup_python_environment $OUTPUT_DIR/stage5_python_setup || exit 1
  echo "   Verification"
  echo "      make plots"
   # Run Python plotting script
  cd "$cfastrepo/Utilities/Python" || exit 1

  run_logged "$OUTPUT_DIR/stage5_run_python_verification" "$CFAST_PYTHON" CFAST_verification_script.py
  check_python_verification

#*** run python validation
  echo "   Validation"
  echo "      make plots"
  # Run Python plotting script
  cd "$cfastrepo/Utilities/Python" || exit 1
  run_logged "$OUTPUT_DIR/stage5_run_python_validation" "$CFAST_PYTHON" CFAST_validation_script.py

  check_python_validation
  check_validation_stats || exit 1
  archive_validation_stats || exit 1

### Stage 6 ###
  echo Building CFAST Tech guide
  cd "$cfastrepo/Manuals/CFAST_Tech_Ref" || exit 1
  run_logged "$OUTPUT_DIR/stage6_cfast_tech_guide" ./make_guide.sh
  check_guide $OUTPUT_DIR/stage6_cfast_tech_guide $cfastrepo/Manuals/CFAST_Tech_Ref CFAST_Tech_Ref.pdf 'CFAST Technical Reference Guide'

  echo Building CFAST User guide
  cd "$cfastrepo/Manuals/CFAST_Users_Guide" || exit 1
  run_logged "$OUTPUT_DIR/stage6_cfast_user_guide" ./make_guide.sh
  check_guide $OUTPUT_DIR/stage6_cfast_user_guide $cfastrepo/Manuals/CFAST_Users_Guide CFAST_Users_Guide.pdf 'CFAST Users Guide'

  echo Building CFAST VV guide
  cd "$cfastrepo/Manuals/CFAST_Validation_Guide" || exit 1
  run_logged "$OUTPUT_DIR/stage6_cfast_vv_guide" ./make_guide.sh
  check_guide $OUTPUT_DIR/stage6_cfast_vv_guide $cfastrepo/Manuals/CFAST_Validation_Guide CFAST_Validation_Guide.pdf 'CFAST Verification and Validation Guide'

  echo Building CFAST Configuration guide
  cd "$cfastrepo/Manuals/CFAST_Configuration_Guide" || exit 1
  run_logged "$OUTPUT_DIR/stage6_cfast_config_guide" ./make_guide.sh
  check_guide $OUTPUT_DIR/stage6_cfast_config_guide $cfastrepo/Manuals/CFAST_Configuration_Guide CFAST_Configuration_Guide.pdf 'CFAST Configuration Guide'

#*** output hashes needed for bundling
  VERSION_LATEST=$GITSTATUS_DIR/VERSION_LATEST
  mkdir -p ${VERSION_LATEST}
  echo $CFAST_SHORTHASH > ${VERSION_LATEST}/CFAST_HASH
  echo $SMV_SHORTHASH   > ${VERSION_LATEST}/SMV_HASH
  echo $CFAST_REV       > ${VERSION_LATEST}/CFAST_REVISION
  echo $SMV_REV         > ${VERSION_LATEST}/SMV_REVISION

### Stage 7 ###
upload_linux_bundle

### Report results ###
set_files_world_readable || exit 1
email_build_status
save_build_status

### save version info if cfastbot passed ###
if [[ ! -e $ERROR_LOG ]] && [[ ! -e $WARNING_LOG ]]; then
  VERSION=$GITSTATUS_DIR/VERSION
  mkdir -p ${VERSION}
  rm -f ${VERSION}/*
  cp ${VERSION_LATEST}/*                                                        ${VERSION}/.
  cp $cfastrepo/Manuals/CFAST_Configuration_Guide/CFAST_Configuration_Guide.pdf ${VERSION}/.
  cp $cfastrepo/Manuals/CFAST_Validation_Guide/CFAST_Validation_Guide.pdf       ${VERSION}/.
  cp $cfastrepo/Manuals/CFAST_Users_Guide/CFAST_Users_Guide.pdf                 ${VERSION}/.
  cp $cfastrepo/Manuals/CFAST_Tech_Ref/CFAST_Tech_Ref.pdf                       ${VERSION}/.
fi
if [[ ! -e $ERROR_LOG && ! -e $WARNING_LOG ]]; then
   cp "$OUTPUT_DIR/revisions.tsv" "$GITSTATUS_DIR/last_successful_revisions.tsv"
   echo cfastbot complete
   exit 0
fi
echo "CFAST CI failed; see $OUTPUT_DIR" >&2
exit 1



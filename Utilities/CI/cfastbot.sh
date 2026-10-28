#!/bin/bash
set -o pipefail
exec 1>&2
export PYTHONUNBUFFERED=1
CI_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
source "$CI_DIR/setup_python.sh"
cur_dir=$CI_DIR
# CFASTbot
# This script runs the CFAST verification/validation suite

# A job disappearing from the scheduler is not proof that it succeeded.
wait_case_manifest() {
  local manifest=$1 label=${2:-Cases} manager job status job_ids= listing accounting
  local i state code total completed running pending confirming failed summary last_summary= next_report=0 now log
  local jobs=() paths=() results=() attempts=() queue_states=() accounting_states=()
  [[ -f $manifest ]] || { echo "Run aborted: missing job manifest: $manifest" >&2; return 1; }
  while IFS=$'\t' read -r manager job status; do
    [[ $manager == slurm && $job =~ ^[0-9]+$ ]] || {
      echo "Run aborted: invalid Slurm manifest: $manifest" >&2; return 1;
    }
    jobs+=("$job"); paths+=("$status"); results+=(""); attempts+=(0)
    job_ids="${job_ids:+$job_ids,}$job"
  done < "$manifest"
  total=${#jobs[@]}
  [[ $total -gt 0 ]] || { echo "Run aborted: no jobs submitted for $label" >&2; return 1; }

  while :; do
    # Query the suite together rather than waiting on each case in turn.
    listing=$(squeue -h -u "$(id -un)" -o '%i|%T') || listing=
    accounting=
    if command -v sacct >/dev/null; then
      accounting=$(sacct -n -P -j "$job_ids" --format=JobID,State,ExitCode) || accounting=
    fi
    queue_states=(); accounting_states=()
    while IFS='|' read -r job state; do
      [[ $job =~ ^[0-9]+$ ]] || continue
      queue_states[job]=$state
    done <<< "$listing"
    while IFS='|' read -r job state code; do
      # Ignore .batch/.extern steps and retain only exact allocation IDs.
      [[ $job =~ ^[0-9]+$ ]] || continue
      accounting_states[job]="$state|$code"
    done <<< "$accounting"

    completed=0; running=0; pending=0; confirming=0; failed=0
    for ((i=0; i<total; i++)); do
      job=${jobs[i]}; status=${paths[i]}
      if [[ -z ${results[i]} ]]; then
        state=
        if [[ -f $status ]]; then
          code=$(cat "$status")
          if [[ $code == 0 ]]; then results[i]=ok
          else state="exit code ${code:-missing}"; fi
        elif [[ -n ${queue_states[job]:-} ]]; then
          attempts[i]=0
          case ${queue_states[job]} in
            PENDING|CONFIGURING) pending=$((pending + 1));;
            *) running=$((running + 1));;
          esac
        else
          case ${accounting_states[job]:-} in
            'COMPLETED|0:0') results[i]=ok;;
            ''|PENDING\|*|RUNNING\|*|COMPLETING\|*|CONFIGURING\|*)
              # Allow accounting and shared filesystem metadata to catch up.
              attempts[i]=$((attempts[i] + 1))
              if [[ ${attempts[i]} -lt 6 ]]; then confirming=$((confirming + 1))
              else state="completion could not be confirmed"; fi;;
            *) state=${accounting_states[job]};;
          esac
        fi
        if [[ -n $state ]]; then
          results[i]=failed
          echo "FAIL: ${status%.exit} (Slurm job $job): $state" >&2
          for log in "${status%.exit}.log" "${status%.exit}.err"; do
            if [[ -s $log ]]; then
              printf '\nCase log: %s\n' "$log" >&2
              tail -n 60 "$log" >&2
            fi
          done
        fi
      fi
      case ${results[i]} in
        ok) completed=$((completed + 1));;
        failed) failed=$((failed + 1));;
      esac
    done

    summary="$label: $completed/$total completed, $running running, $pending pending, $confirming confirming, $failed failed"
    now=$(date +%s)
    if [[ $summary != "$last_summary" || $now -ge $next_report ]]; then
      echo "$summary"
      last_summary=$summary; next_report=$((now + 30))
    fi
    # Collect all jobs already submitted to this suite before reporting failure.
    # Later pipeline stages still stop; the email includes every failed case.
    if [[ $((completed + failed)) -eq $total ]]; then
      [[ $failed == 0 ]]
      return $?
    fi
    if declare -F check_time_limit >/dev/null; then check_time_limit; fi
    sleep "${CFAST_CI_POLL_SECONDS:-5}"
  done
}

run_logged()
{
   local log=$1 result tee_pid tee_result stream_dir
   shift
   # A named pipe streams output while keeping setup functions in this shell.
   stream_dir=$(mktemp -d "${log}.stream.XXXXXX") || return 1
   mkfifo "$stream_dir/output" || { rmdir "$stream_dir"; return 1; }
   tee -a "$log" < "$stream_dir/output" >&2 &
   tee_pid=$!
   "$@" > "$stream_dir/output" 2>&1
   result=$?
   wait "$tee_pid"
   tee_result=$?
   rm -f "$stream_dir/output"
   rmdir "$stream_dir"
   if [[ $result == 0 ]]; then result=$tee_result; fi
   if [[ $result != 0 ]]; then
      echo "Command failed (exit $result): $*" | tee -a "$ERROR_LOG" >&2
      cat "$log" >> "$ERROR_LOG"
   fi
   return "$result"
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
         printf 'Warning: CFASTbot exceeded its %s-second time limit in Stage %s; the run continued.\n' \
            "$TIME_LIMIT" "$TIME_LIMIT_STAGE" >> "$WARNING_LOG"
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
      run_logged "$setup_log" echo "Python environment already active."
      return 0
   fi

   echo "Setting up Python environment"
   run_logged "$setup_log" setup_cfast_python "$reporoot" || return 1
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
   cd "$smvrepo/Build/smokeview/${smv_compiler}_${platform}" || exit 1
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
   cd "$smvrepo/Build/smokeview/${smv_compiler}_${platform}" || exit 1
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
   local result=0 mode=release option
   for option in "$@"; do
      case "$option" in
         -d) mode="debug initialization (2 iterations)";;
         --test-UI) mode="UI import/save (no simulation)";;
      esac
   done
   if [[ -n $QUEUE ]]; then
      set -- "$@" -q "$QUEUE"
      echo "Submitting $suite $mode cases"
   else
      echo "Running $suite $mode cases locally"
   fi
   export CFAST_JOB_MANIFEST="$run_log.jobs"
   : > "$CFAST_JOB_MANIFEST"
   run_logged "$run_log" "$CI_DIR/Run_CFAST_Cases.sh" --repo-root "$cfastrepo" --suite "$suite" "$@" || result=1
   TIME_LIMIT_STAGE="3 $suite cases"
   if [[ -n $QUEUE ]]; then
      run_logged "$run_log" wait_case_manifest "$CFAST_JOB_MANIFEST" "$suite $mode" || result=1
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

   submit_suite Verification "$verification_log" -I "$compiler" -S "$smvrepo" -m 2 -d -j "$JOBPREFIX" || return 1
   cat "$verification_log" >> $OUTPUT_DIR/stage3_run_debug 2>&1

   submit_suite Validation "$validation_log" -I "$compiler" -S "$smvrepo" -m 2 -d -j "$JOBPREFIX" || return 1
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

   submit_suite Verification "$verification_log" -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" || return 1
   cat "$verification_log" >> $OUTPUT_DIR/stage3_run_release 2>&1

   submit_suite Validation "$validation_log" -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" || return 1
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

   echo '   CEditQt import/save checks (no simulation)'
   echo 'Running CEditQt V&V UI import/rewrite tests' > $OUTPUT_DIR/stage3_run_release_ceditqt 2>&1

   setup_python_environment $OUTPUT_DIR/stage3_ceditqt_python_setup || return 1

   submit_suite Verification "$verification_log" --test-UI -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" || return 1
   cat "$verification_log" >> $OUTPUT_DIR/stage3_run_release_ceditqt 2>&1

   submit_suite Validation "$validation_log" --test-UI -I "$compiler" -S "$smvrepo" -j "$JOBPREFIX" || return 1
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
   local comparison_status diff_status
   cd "$cfastrepo/Utilities/Python" || exit 1

   STATS_FILE_BASENAME=validation_scatterplot_output

   BASELINE_STATS_FILE=$cfastrepo/Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES/Scatterplots/${STATS_FILE_BASENAME}_baseline.csv
   CURRENT_STATS_FILE=$cfastrepo/Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES/Scatterplots/${STATS_FILE_BASENAME}.csv

   # Use the FDS validation rules with a 1% CFAST tolerance: changes fail only when
   # Sigma_Model increases or Bias moves farther from one.
   "$CFAST_PYTHON" - "$BASELINE_STATS_FILE" "$CURRENT_STATS_FILE" > "$VALIDATION_STATS_LOG" 2>&1 <<'PY'
import csv
import sys
from decimal import Decimal, InvalidOperation

def read_statistics(path):
    with open(path, newline='') as stream:
        reader = csv.DictReader(stream)
        if not {'Quantity', 'Sigma_Model', 'Bias'}.issubset(reader.fieldnames or []):
            raise ValueError(f'missing statistics columns in {path}')
        rows = {}
        for row in reader:
            quantity = row['Quantity']
            if not quantity or quantity in rows:
                raise ValueError(f'missing or duplicate quantity in {path}: {quantity}')
            rows[quantity] = row
        if not rows:
            raise ValueError(f'no statistics in {path}')
        return list(reader.fieldnames), rows

try:
    _, baseline = read_statistics(sys.argv[1])
    fieldnames, current = read_statistics(sys.argv[2])
except (OSError, ValueError, csv.Error) as exc:
    print(f'Error: Cannot compare validation statistics: {exc}')
    sys.exit(1)

metrics = ('Sigma_Model', 'Bias')
difference_columns = [metric + '_Relative_Difference' for metric in metrics]
# Match FDS CSV output, refreshing existing columns when the check is rerun.
fieldnames += [column for column in difference_columns if column not in fieldnames]
for row in current.values():
    for column in difference_columns:
        row[column] = ''

failed = False
for quantity in sorted(baseline.keys() | current.keys()):
    if quantity not in baseline or quantity not in current:
        print(f'Error: {quantity}: missing from validation output or baseline.')
        failed = True
        continue
    for metric, column in zip(metrics, difference_columns):
        try:
            value = Decimal(current[quantity][metric])
            reference = Decimal(baseline[quantity][metric])
            if not value.is_finite() or not reference.is_finite():
                raise ValueError('non-finite statistic')
        except (KeyError, TypeError, ValueError, InvalidOperation):
            print(f'Error: {quantity}: invalid {metric} in validation output or baseline.')
            failed = True
            continue
        if reference == 0:
            difference = Decimal(0) if value == 0 else Decimal('Infinity').copy_sign(value)
        else:
            difference = (value - reference) / abs(reference)
        current[quantity][column] = f'{difference:.6f}'
        # Decimal preserves the strict boundary: exactly 1% is not a failure.
        if abs(difference) > Decimal('0.01'):
            current_error = value if metric == 'Sigma_Model' else abs(value - 1)
            baseline_error = reference if metric == 'Sigma_Model' else abs(reference - 1)
            worse = current_error > baseline_error
            failed |= worse
            severity = 'Error' if worse else 'Warning'
            print(f'{severity}: {quantity}: {metric} relative difference {difference:.2%} '
                  f'exceeds 1% in magnitude (current={value}, baseline={reference}).')

with open(sys.argv[2], 'w', newline='') as stream:
    writer = csv.DictWriter(stream, fieldnames=fieldnames, lineterminator='\n')
    writer.writeheader()
    writer.writerows(current.values())
sys.exit(1 if failed else 0)
PY
   comparison_status=$?
   # Include the full diff only when a metric needs attention.
   if [[ -s $VALIDATION_STATS_LOG && -f $BASELINE_STATS_FILE && -f $CURRENT_STATS_FILE ]]; then
      diff -u --label baseline --label "Revision ${GIT_REVISION}" \
         <(sed 's/"//g' "$BASELINE_STATS_FILE") <(sed 's/"//g' "$CURRENT_STATS_FILE") >> "$VALIDATION_STATS_LOG"
      diff_status=$?
      if [[ $diff_status -gt 1 ]]; then
         echo "Error: Could not compare validation statistics." >> "$VALIDATION_STATS_LOG"
         comparison_status=1
      fi
   fi
   if [[ $comparison_status != 0 ]]; then
      echo "Errors from Stage 5 - Validation statistics:" >> "$ERROR_LOG"
      cat "$VALIDATION_STATS_LOG" >> "$ERROR_LOG"
      return 1
   fi
   if [[ -s $VALIDATION_STATS_LOG ]]; then
      echo "Warnings from Stage 5 - Validation statistics changed from baseline:" >> "$WARNING_LOG"
      cat "$VALIDATION_STATS_LOG" >> "$WARNING_LOG"
      echo "" >> "$WARNING_LOG"
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
   if run_logged "$bundle_log" "$cedit_script" --python "$CFAST_PYTHON" &&
      run_logged "$bundle_log" "$bundle_script" --no-update-repos --no-build-cfast --no-build-smokeview \
         --no-build-manuals --no-upload-manuals \
         --cfast-exe "$cfastrepo/Build/CFAST/${compiler}_linux/cfast8_linux" \
         --smokeview-exe "$smvrepo/Build/smokeview/intel_linux/smokeview_linux" \
         --output-dir "$OUTPUT_DIR/bundles" --stage-dir "$OUTPUT_DIR/bundle_stage" \
         --upload --upload-release-repo "$GH_OWNER/$GH_REPO" \
         --upload-release-tag "$GH_CFAST_TAG"; then
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
   local label subject
   stop_time=$(date)
   if [[ $platform == osx ]]; then
      IFORT_VERSION=$(gfortran --version 2>&1)
   else
      IFORT_VERSION=$(ifx -v 2>&1)
   fi
   {
      printf '\nHost: %s\nrepo: %s\nLogs: %s\n' "$hostname" "$cfastrepo" "$OUTPUT_DIR"
      printf 'Fortran: %s\n\n%s\n' "$IFORT_VERSION" "$BOT_REVISION"
      printf 'CFAST: %s\nEXP: %s\nFDS: %s\nSMV: %s\n' \
         "$CFAST_REVISION" "$EXP_REVISION" "$FDS_REVISION" "$SMV_REVISION"
      printf '\nStart Time: %s\nStop Time: %s\n' "$start_time" "$stop_time"
      [[ -z $total_time ]] || printf 'Run Time: %s\n' "$total_time"
      [[ -z $GITURL ]] || printf 'Linux bundle, Manuals: %s\n' "$GITURL"
   } >> "$TIME_LOG"
   if [[ -s $ERROR_LOG ]]; then
      label='failure'
      [[ ! -s $WARNING_LOG ]] || label='failure and warnings'
   elif [[ -s $WARNING_LOG ]]; then
      label='success with warnings'
   else
      label='success'
   fi
   subject="CFASTbot build $label on ${hostname}. Revision ${GIT_REVISION}."
   # Keep the exact body and mail diagnostics even if delivery fails.
   {
      [[ ! -s $ERROR_LOG ]] || cat "$ERROR_LOG"
      [[ ! -s $WARNING_LOG ]] || cat "$WARNING_LOG"
      cat "$TIME_LOG"
   } > "$OUTPUT_DIR/email_body.txt"
   # A standalone Mac does not need a configured mail transport.
   if [[ $platform == osx && -z $mailTo ]]; then
      echo "Build report: $OUTPUT_DIR/email_body.txt"
      return 0
   fi
   if ! mail -S posix -S "reply-to=${mailTo// /,}" -s "$subject" $mailTo < "$OUTPUT_DIR/email_body.txt" > "$OUTPUT_DIR/email.log" 2>&1; then
      echo "Could not send CFASTbot status email; see $OUTPUT_DIR/email.log" >&2
      return 1
   fi
}

# Run reporting on every normal or failed pipeline exit, without running uploads
# or later simulation/plotting stages after a failed prerequisite.
finalize_build_status()
{
   local result=$?
   trap - EXIT
   if [[ $result != 0 ]]; then
      printf '\nCFASTbot stopped with exit code %s. Logs: %s\n' "$result" "$OUTPUT_DIR" >> "$ERROR_LOG"
   fi
   if [[ -s $ERROR_LOG ]]; then
      THIS_CFAST_FAILED=1
      [[ $result != 0 ]] || result=1
   fi
   if ! email_build_status; then
      echo "Status email delivery failed; see $OUTPUT_DIR/email.log and email_body.txt" >> "$ERROR_LOG"
      THIS_CFAST_FAILED=1
      [[ $result != 0 ]] || result=1
   fi
   printf '%s\n' "$THIS_CFAST_FAILED" > "$CFAST_STATUS_FILE"
   save_build_status || { [[ $result != 0 ]] || result=1; }
   exit "$result"
}

upload_guides()
{
   if [[ "$UPLOAD" == "1" && -e $GUIDES2GH && ! -s $ERROR_LOG && ! -s $WARNING_LOG ]]; then
      run_logged "$OUTPUT_DIR/stage7_upload" "$GUIDES2GH" "$cfastrepo/Manuals" "$GITSTATUS_DIR/VERSION_LATEST" || return 1
      GITURL=https://github.com/$GH_OWNER/$GH_REPO/releases/tag/$GH_CFAST_TAG
   fi
   return 0
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
QUEUE=
platform=linux
cfast_platform=linux
compiler=intel
fortran=ifx
smv_compiler=intel
if [[ $(uname) == Darwin ]]; then
   platform=osx
   cfast_platform=macos
   compiler=gnu
   fortran=gfortran
   smv_compiler=clang
fi
export platform cfast_platform
UPLOAD=
PYTHON_ENV_ACTIVE=
while (($#)); do
   case "$1" in
      -q) QUEUE=$2; shift 2;;
      -m) mailTo=$2; shift 2;;
      -U) UPLOAD=1; shift;;
      *) echo "Unknown driver option: $1" >&2; exit 2;;
   esac
done
cfastrepo=$reporoot/cfast
fdsrepo=$reporoot/fds
smvrepo=$reporoot/smv
exprepo=$reporoot/exp
mailTo=${mailTo:-${mailToCFAST:-}}
if [[ -z $mailTo && $platform != osx ]]; then mailTo=$(git -C "$cfastrepo" config user.email || whoami); fi
hostname=$(hostname)
start_time=$(date)
START_TIME=$(date +%s)
GIT_REVISION=$(git -C "$cfastrepo" rev-parse --short HEAD 2>/dev/null || echo unknown)
THIS_CFAST_FAILED=0
CFAST_STATUS_FILE=$GITSTATUS_DIR/cfast_status
trap finalize_build_status EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
for repo in "$cfastrepo" "$fdsrepo" "$smvrepo" "$exprepo"; do
   [[ -d $repo ]] || { echo "Missing repository: $repo" | tee -a "$ERROR_LOG" >&2; exit 1; }
done
: > "$OUTPUT_DIR/revisions.tsv"
for name in cfast fds smv exp; do
   printf '%s\t%s\n' "$name" "$(git -C "$reporoot/$name" rev-parse HEAD)" >> "$OUTPUT_DIR/revisions.tsv"
   git -C "$reporoot/$name" submodule status --recursive >> "$OUTPUT_DIR/revisions.tsv"
done
printf 'ci_tooling\t%s\n' "${CFAST_CI_TOOLING_REVISION:-$(git -C "$cfastrepo" rev-parse HEAD)}" >> "$OUTPUT_DIR/revisions.tsv"
missing=0
for program in python3 "$fortran" pdflatex biber; do
   command -v "$program" >/dev/null || { echo "Missing CI prerequisite: $program" | tee -a "$ERROR_LOG" >&2; missing=1; }
done
if [[ -n $QUEUE ]]; then
   for program in sbatch squeue scancel; do
      command -v "$program" >/dev/null || { echo "Missing Slurm command: $program." | tee -a "$ERROR_LOG" >&2; missing=1; }
   done
fi
[[ $missing == 0 ]] || exit 1

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
# If CFASTbot exceeds the time limit, record a warning for the final report.
# This does not terminate CFASTbot.
# This check runs during Stages 3 and 5.

# Set time limit
TIME_LIMIT=14400
TIME_LIMIT_EMAIL_NOTIFICATION="unsent"


setup_python_environment "$OUTPUT_DIR/stage0_python_setup" || exit 1

### Stage 1 ###

BOT_REVISION="CI tooling: ${CFAST_CI_TOOLING_REVISION:-$(git -C "$cfastrepo" describe --tags --always --dirty)}"

cd "$reporoot/exp" || exit 1
EXP_REVISION=`git describe --tags --always --abbrev=7 --dirty --long`

cd "$reporoot/fds" || exit 1
FDS_REVISION=`git describe --tags --always --abbrev=7 --dirty --long`

cd "$reporoot/cfast" || exit 1
CFAST_REVISION=`git describe --tags --always --abbrev=7 --dirty --long`
CFAST_SHORTHASH=`git rev-parse --short HEAD`
GIT_REVISION=$CFAST_SHORTHASH
# CFAST_REV same as CFAST_REVISION without the hash on the end
CFAST_REV=`git describe --tags --always | sed 's/-g[0-9a-f]*$//'`

cd "$reporoot/smv" || exit 1
SMV_REVISION=`git describe --tags --always --abbrev=7 --dirty --long`
SMV_SHORTHASH=`git rev-parse --short HEAD`
# SMV_REV same as SMV_REVISION without the hash on the end
SMV_REV=`git describe --tags --always | sed 's/-g[0-9a-f]*$//'`

cd "$cur_dir" || exit 1

### Stage 2 ###

#*** build cfast debug cfast
echo "Building"
echo "   cfast"
echo "      $compiler debug"
cd "$cfastrepo/Build/CFAST/${compiler}_${cfast_platform}_db" || exit 1
run_logged "$OUTPUT_DIR/stage2_build_cfast_debug" ./make_cfast.sh --clean-cfast || exit 1
check_compile_cfast_db || exit 1

#*** build release cfast
echo "      release"
cd "$cfastrepo/Build/CFAST/${compiler}_${cfast_platform}" || exit 1
run_logged "$OUTPUT_DIR/stage2_build_cfast_release" ./make_cfast.sh --clean-cfast || exit 1
check_compile_cfast || exit 1

#*** build smokeview libraries
cd "$smvrepo/Build/LIBS/${smv_compiler}_${platform}" || exit 1
echo 'Building Smokeview libraries' >> $OUTPUT_DIR/stage2_build_smv_util 2>&1
echo "   smokeview libraries"
run_logged "$OUTPUT_DIR/stage2_build_smv_util" ./make_LIBS.sh || exit 1

#*** build debug smokeview
echo "   smokeview"
echo "      debug"
cd "$smvrepo/Build/smokeview/${smv_compiler}_${platform}" || exit 1
run_logged "$OUTPUT_DIR/stage2_build_smv_debug" ./make_smokeview_db.sh || exit 1
check_compile_smv_db || exit 1

#*** build release smokeview
echo "      release"
cd "$smvrepo/Build/smokeview/${smv_compiler}_${platform}" || exit 1
run_logged "$OUTPUT_DIR/stage2_build_smv_release" ./make_smokeview.sh || exit 1
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
   run_ceditqt_cases_release || exit 1
   check_ceditqt_cases_release || exit 1
fi

### Stage 4 ###
if [[ $stage2_build_cfast_release_success && $stage2_build_smv_release_success ]] ; then
   echo "Generating smokeview images"
   cd "$cfastrepo/Validation/scripts" || exit 1
   if [[ $platform == osx ]]; then
      # Native Cocoa Smokeview needs neither Xvfb nor the old FDS/SMV layout.
      export CFAST_PICTURE_EXE="$smvrepo/Build/smokeview/clang_osx/smokeview_osx"
      export CFAST_PICTURE_ROOT="$cfastrepo/Validation"
      export CFAST_PICTURE_OUTPUT="$cfastrepo/Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES"
      export CFAST_PICTURE_BINDIR="$smvrepo/for_bundle"
      mkdir -p "$CFAST_PICTURE_OUTPUT"
      cfast_picture() (
         [[ $# == 3 && $1 == -d ]] || return 2
         cd "$CFAST_PICTURE_ROOT/$2" || return 1
         local base=${3%.*}
         [[ -f $base.smv && -f $base.ssf ]] || return 1
         "$CFAST_PICTURE_EXE" -bindir "$CFAST_PICTURE_BINDIR" -scriptrenderdir "$CFAST_PICTURE_OUTPUT" -runscript "$base" > "$base.serr" 2>&1
      )
      export -f cfast_picture
      export RUNSMV=cfast_picture
      run_logged "$OUTPUT_DIR/stage4_make_pictures" bash -e ./CFAST_Pictures.sh || exit 1
   else
      run_logged "$OUTPUT_DIR/stage4_make_pictures" ./Make_CFAST_Pictures.sh -I intel
   fi
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
  validation_stats_status=0
  check_validation_stats || validation_stats_status=$?
  archive_validation_stats || exit 1
  [[ $validation_stats_status == 0 ]] || exit 1

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
upload_linux_bundle || exit 1
upload_guides || exit 1

### Report results (the EXIT trap sends status on success or failure) ###
set_files_world_readable || exit 1

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
if [[ -s $ERROR_LOG ]]; then
   echo "CFAST CI failed; see $OUTPUT_DIR" >&2
   exit 1
fi
echo "CFAST CI completed with warnings; see $OUTPUT_DIR" >&2
exit 0

#!/usr/bin/env bash
# A job disappearing from the scheduler is not proof that it succeeded.
wait_case_manifest() {
  local manifest=$1 manager job status active rc=0
  [[ -f $manifest ]] || { echo "Run aborted: missing job manifest: $manifest" >&2; return 1; }
  while IFS=$'\t' read -r manager job status; do
    while [[ ! -f $status ]]; do
      active=0
      case "$manager" in
        slurm)
          if ! listing=$(squeue -h -j "$job" -o '%i'); then
            echo "Run aborted: cannot query Slurm job $job" >&2; rc=1; break
          fi
          [[ -z $listing ]] || active=1;;
        pbs) if qstat "$job" >/dev/null 2>&1; then active=1; fi;;
        local) if [[ $job != 0 ]] && kill -0 "$job" 2>/dev/null; then active=1; fi;;
        *) echo "Run aborted: unknown scheduler: $manager" >&2; return 1;;
      esac
      if [[ $active == 0 ]]; then
        # Allow shared filesystem metadata to catch up after scheduler completion.
        sleep "${CFAST_CI_STATUS_GRACE:-2}"
        [[ -f $status ]] || { echo "Run aborted: job $job ended without a status file: $status" >&2; rc=1; break; }
      else
        if declare -F check_time_limit >/dev/null; then check_time_limit; fi
        sleep "${CFAST_CI_POLL_SECONDS:-5}"
      fi
    done
    if [[ -f $status && $(cat "$status") != 0 ]]; then
      echo "Run aborted: job $job failed ($(cat "$status")): $status" >&2; rc=1
    fi
  done < "$manifest"
  return "$rc"
}

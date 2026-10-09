#!/usr/bin/env bash
# Run the CFAST pipeline on existing checkouts. Repository preparation is external.
set -euo pipefail
CI_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$CI_DIR/../../.." && pwd)
state=${CFAST_CI_STATE_DIR:-$HOME/.cfastbot}
queue=batch; args=()
usage() {
  cat <<'HELP'
Usage: run_cfastbot.sh [-q queue] [-U] [-m address]
  -q queue    Slurm partition (default: batch)
  -U          Upload guides and the Linux bundle
  -m address  Notification email
  -h, --help  Show help
Run update_repos.sh in the repository collection before starting CFASTbot.
HELP
}
while (($#)); do
  case "$1" in
    -q|-m)
      [[ $# -ge 2 && -n $2 && $2 != -* ]] || { echo "$1 requires a value." >&2; exit 2; }
      if [[ $1 == -q ]]; then queue=$2; else args+=(-m "$2"); fi
      shift 2;;
    -U) args+=("$1"); shift;;
    -h|--help) usage; exit 0;; *) echo "Unknown option: $1" >&2; usage >&2; exit 2;;
  esac
done
root=$(cd "$root" && pwd -P)
mkdir -p "$state"
state=$(cd "$state" && pwd -P)
for repo in cfast fds exp smv; do
  case "$state/" in "$root/$repo/"*) echo 'CI state must be outside repository checkouts.' >&2; exit 1;; esac
done
export CFAST_CI_STATE_DIR=$state
output="$state/runs/$(date +%Y%m%d-%H%M%S)-$$"
mkdir -p "$output"
printf '%s\n' "$output" > "$state/latest_run"
export CFAST_CI_OUTPUT_DIR=$output
printf 'CFASTbot logs: %s\n' "$output" >&2
child=
terminate_tree() {
  local parent=$1 pid
  for pid in $(pgrep -P "$parent" 2>/dev/null || true); do terminate_tree "$pid"; done
  kill -TERM "$parent" 2>/dev/null || true
}
cancel_jobs() {
  local manifest manager job status
  for manifest in "$output"/*.jobs; do
    [[ -f $manifest ]] || continue
    while IFS=$'\t' read -r manager job status; do
      [[ -f $status ]] && continue
      [[ $manager != slurm ]] || scancel "$job" || true
    done < "$manifest"
  done
}
interrupt() { cancel_jobs; if [[ -n $child ]]; then terminate_tree "$child"; wait "$child" 2>/dev/null || true; fi; }
trap 'interrupt; exit 130' INT
trap 'interrupt; exit 143' TERM
bash "$CI_DIR/cfastbot.sh" -q "$queue" "${args[@]}" &
child=$!
rc=0
wait "$child" || rc=$?
printf '%s\n' "$rc" > "$output/exit_code"
if [[ $rc != 0 ]]; then
  cancel_jobs
  echo "CFASTbot failed (exit $rc); logs: $output" >&2
fi
exit "$rc"

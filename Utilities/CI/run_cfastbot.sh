#!/usr/bin/env bash
# Run the CFAST pipeline on existing checkouts. Repository preparation is external.
set -euo pipefail
CI_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$CI_DIR/../../.." && pwd)
state=${CFAST_CI_STATE_DIR:-$HOME/.cfastbot}
force=0; stop=0; preflight=0; queue=; args=()
usage() {
  cat <<'HELP'
Usage: run_cfastbot.sh [options]
  --root directory Parent of cfast, fds, exp, and smv
  -q queue         Scheduler partition, terminal, or none
  -I intel|gnu     CFAST compiler (default: intel; Smokeview uses Intel builds)
  --test-UI        Submit CEditQt import/rewrite tests
  -a               Run only when the tested repository revisions change
  -m address       Notification email
  -o owner         GitHub upload owner
  -r repository    GitHub upload repository
  -U               Upload guides and the Linux bundle
  -f               Remove a stale lock (never bypass a running CI job)
  -k               Terminate the active CI run and its submitted jobs
  --preflight      Check prerequisites without building, submitting, or publishing
  -h               Show help
For fetch/clean or release-config runs, use bot/Scripts/run_cfast_ci.sh -C or -F.
HELP
}
while (($#)); do
  case "$1" in
    --root) root=$2; shift 2;;
    -q|-I|-m) args+=("$1" "$2"); [[ $1 != -q ]] || queue=$2; shift 2;;
    -o) export GH_OWNER=$2; shift 2;; -r) export GH_REPO=$2; shift 2;;
    -a|-U|--test-UI) args+=("$1"); shift;;
    -f) force=1; shift;; -k) stop=1; shift;; --preflight) preflight=1; shift;;
    -C|-F) echo 'Use bot/Scripts/run_cfast_ci.sh for checkout preparation.' >&2; exit 2;;
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
lock="$state/run.lock"
if [[ $stop == 1 ]]; then
  if [[ -f $lock/pid ]]; then
    owner=$(cat "$lock/pid")
    [[ $owner =~ ^[0-9]+$ ]] || { echo 'Invalid lock PID.' >&2; exit 1; }
    kill -TERM "$owner"
    echo "Requested termination of CFAST CI ($owner)."
  else echo 'CFAST CI is not running.'; fi
  exit 0
fi
owns_lock=0
if [[ -n ${CFAST_CI_LOCK_PID:-} && -f $lock/pid && $(cat "$lock/pid") == "$CFAST_CI_LOCK_PID" ]] && kill -0 "$CFAST_CI_LOCK_PID" 2>/dev/null; then
  : # The external preparation launcher owns this lock through pipeline completion.
else
  if [[ $force == 1 && -f $lock/pid ]]; then
    owner=$(cat "$lock/pid")
    if [[ $owner =~ ^[0-9]+$ ]] && ! kill -0 "$owner" 2>/dev/null; then
      rm -f "$lock/pid" "$lock/driver_pid"; rmdir "$lock"
    fi
  fi
  mkdir "$lock" 2>/dev/null || { echo "CFAST CI is locked: $lock" >&2; exit 1; }
  echo $$ > "$lock/pid"; owns_lock=1
fi
output="$state/runs/$(date +%Y%m%d-%H%M%S)-$$"
mkdir -p "$output"
printf '%s\n' "$output" > "$state/latest_run"
export CFAST_CI_OUTPUT_DIR=$output
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
      case "$manager" in slurm) scancel "$job" || true;; pbs) qdel "$job" || true;; local) [[ $job == 0 ]] || terminate_tree "$job";; esac
    done < "$manifest"
  done
}
cleanup() {
  if [[ $owns_lock == 1 ]]; then rm -f "$lock/pid" "$lock/driver_pid"; rmdir "$lock"; fi
}
interrupt() { cancel_jobs; if [[ -n $child ]]; then terminate_tree "$child"; wait "$child" 2>/dev/null || true; fi; }
trap cleanup EXIT
trap 'interrupt; exit 130' INT
trap 'interrupt; exit 143' TERM
if [[ -z $queue ]]; then
  if command -v sinfo >/dev/null; then queue=$(sinfo -h -o '%P' | awk '/\*/ {gsub(/\*/, ""); print; exit}'); fi
  queue=${queue:-terminal}; args+=(-q "$queue")
fi
if [[ $preflight == 1 ]]; then args+=(--preflight); fi
bash "$CI_DIR/cfastbot.sh" -r "$root" "${args[@]}" &
child=$!
echo "$child" > "$lock/driver_pid"
rc=0
wait "$child" || rc=$?
printf '%s\n' "$rc" > "$output/exit_code"
if [[ $rc != 0 ]]; then cancel_jobs; fi
exit "$rc"

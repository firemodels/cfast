#!/usr/bin/env bash
# Run the selected suite with one shared launcher and the suite's own case list.
set -euo pipefail
CI_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=${CFAST_REPO:-$(cd "$CI_DIR/../.." && pwd)}
suite=; compiler=intel; debug=; queue=batch; prefix=; timing=0; ui=0; preview=0; selected=; exe=
launch_args=()
usage() {
  cat <<'HELP'
Usage: Run_CFAST_Cases.sh --suite Verification|Validation [options]
  -I intel|gnu    Compiler (default: intel)
  -d              Debug executable
  -e executable   Override executable
  -q queue        Slurm partition (default: batch)
  -j prefix       Job-name prefix
  -m iterations   Stop after the specified iteration count
  -s              Stop the suite's cases
  -t              Report recorded execution times
  -v              Print job scripts without submitting
  --test-UI       Import/rewrite inputs through CEditQt
  --case filename Run only matching filenames (or stems)
  --repo-root dir CFAST checkout to test
HELP
}
unset STOPFDS STOPFDSMAXITER
while (($#)); do
  case "$1" in
    --suite|--repo-root|--case|-I|-e|-q|-j|-m|-S)
      [[ $# -ge 2 ]] || { usage >&2; exit 2; }
      case "$1" in
        --suite) suite=$2;; --repo-root) repo=$2;; --case) selected=$2;; -I) compiler=$2;;
        -e) exe=$2;; -q) queue=$2;; -j) prefix=$2;; -m) export STOPFDSMAXITER=$2;; -S) :;;
      esac; shift 2;;
    -d) debug=_db; shift;; -s) export STOPFDS=1; shift;; -t) timing=1; shift;;
    -v) preview=1; shift;; --test-UI) ui=1; shift;; -h|--help) usage; exit 0;;
    *) echo "Unknown option: $1" >&2; exit 2;;
  esac
done
case "$suite" in verification|Verification) suite=Verification;; validation|Validation) suite=Validation;; *) usage >&2; exit 2;; esac
case "$compiler" in intel|gnu) :;; *) echo 'Compiler must be intel or gnu.' >&2; exit 2;; esac
repo=$(cd "$repo" && pwd)
case $(uname) in Darwin) platform=macos;; Linux) platform=linux;; MINGW*|MSYS*|CYGWIN*) platform=win;; *) echo 'Unsupported platform.' >&2; exit 1;; esac
export CFAST_REPO=$repo BASEDIR="$repo/$suite" SVNROOT=$repo
export CFAST=${exe:-$repo/Build/CFAST/${compiler}_${platform}${debug}/cfast8_${platform}${debug}}
match_file=$(mktemp "${TMPDIR:-/tmp}/cfast-cases.XXXXXX")
trap 'rm -f "$match_file"' EXIT
export CFAST_MATCH_FILE=$match_file
export CFAST_LAUNCHER="$CI_DIR/../scripts/qcfast.sh"
export CFAST_QUEUE=$queue CFAST_PREFIX=$prefix CFAST_TIMING=$timing CFAST_UI=$ui CFAST_PREVIEW=$preview CFAST_SELECTED=$selected
run_cfast_case() {
  local args=("$@") input=${!#} directory=. i
  [[ -z $CFAST_SELECTED || $input == "$CFAST_SELECTED" || ${input%.*} == "$CFAST_SELECTED" ]] || return 0
  printf "%s\n" "$input" >> "$CFAST_MATCH_FILE"
  if [[ $CFAST_TIMING == 1 ]]; then
    for ((i=0; i<${#args[@]}; i++)); do [[ ${args[i]} != -d ]] || directory=${args[i+1]}; done
    local log="$BASEDIR/$directory/${input%.*}.log"
    [[ -f $log ]] || { echo "Run aborted: missing log: $log" >&2; return 1; }
    awk -v name="$input" '/execution time/ {t=$5} /time steps/ {s=$5} END {print name "," t "," s}' "$log"
    return
  fi
  local options=(-q "$CFAST_QUEUE" -j "$CFAST_PREFIX" -e "$CFAST")
  [[ $CFAST_UI == 0 ]] || options+=(--test-UI)
  [[ $CFAST_PREVIEW == 0 ]] || options+=(-v)
  "$CFAST_LAUNCHER" "${options[@]}" "$@"
}
export -f run_cfast_case
export RUNCFAST=run_cfast_case
cd "$BASEDIR"
# -e propagates the first failed submission instead of masking it with a later success.
bash -e scripts/CFAST_Cases.sh
[[ -s $match_file ]] || { echo "No matching cases in $suite: $selected" >&2; exit 1; }

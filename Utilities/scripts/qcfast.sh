#!/usr/bin/env bash
# Submit one CFAST or CEditQt case. Used by both V&V suites.
set -euo pipefail
usage() {
  cat <<'HELP'
Usage: qcfast.sh [-d directory] [-e executable] [-q queue] [options] input.in
  -q queue     Slurm/PBS queue; terminal runs locally, none runs locally in background
  -e path      CFAST executable (required except for --test-UI or stopping)
  -j prefix    Scheduler job-name prefix
  -c           Pass the input basename without its extension
  -s           Write the case stop file
  -v           Print the job script without submitting or changing case outputs
  -V           Pass -V to CFAST
  -E           Capture stderr (also the default for local runs)
  --test-UI    Import/rewrite through CEditQt without running CFAST
  -h           Show this help
HELP
}
queue=batch; dir=.; exe=${CFAST:-}; prefix=; strip=0; stop=0; preview=0; version=0; ui=0
while (($#)); do
  case "$1" in
    -d|-e|-q|-j) [[ $# -ge 2 ]] || { usage >&2; exit 2; }
      case "$1" in -d) dir=$2;; -e) exe=$2;; -q) queue=$2;; -j) prefix=$2;; esac; shift 2;;
    -c) strip=1; shift;; -s) stop=1; shift;; -v) preview=1; shift;; -V) version=1; shift;;
    -E|-b) shift;; # -b was accepted by the former V&V launcher; -e selects the build.
    --test-UI) ui=1; shift;; -h|--help) usage; exit 0;;
    --) shift; break;; -*) echo "Unknown option: $1" >&2; exit 2;; *) break;;
  esac
done
[[ $# == 1 ]] || { usage >&2; exit 2; }
input=$1
[[ "$input" != */* ]] || { echo 'Use -d for the input directory.' >&2; exit 2; }
dir=$(cd "$dir" && pwd)
base=${input%.*}
job_name=$(printf '%s' "$prefix$base" | tr -c '[:alnum:]_.-' '_')
[[ -f "$dir/$input" ]] || { echo "Run aborted: input not found: $dir/$input" >&2; exit 1; }
if [[ $stop == 1 || -n ${STOPFDS:-} ]]; then
  [[ $preview == 1 ]] || touch "$dir/$base.stop"
  exit 0
fi
suffix=
if [[ $ui == 1 ]]; then
  suffix=.ui
  repo=${CFAST_REPO:?CFAST_REPO is required for UI tests}
  command=("${CFAST_PYTHON:-python}" "$repo/Source/CeditQt/run_verification_ui_tests.py"
    --mode rewrite --repo-root "$repo" --input "$dir/$input" --skip-helpers)
else
  [[ -n $exe ]] || { echo 'Run aborted: specify -e executable.' >&2; exit 1; }
  exe=$(command -v "$exe") || { echo "Run aborted: executable not found: $exe" >&2; exit 1; }
  [[ -x $exe ]] || { echo "Run aborted: not executable: $exe" >&2; exit 1; }
  [[ $exe == /* ]] || exe="$(cd "$(dirname "$exe")" && pwd)/$(basename "$exe")"
  argument=$input; [[ $strip == 0 ]] || argument=$base
  command=("$exe" "$argument")
  [[ $version == 0 ]] || command+=(-V)
fi
log="$dir/$base$suffix.log"; err="$dir/$base$suffix.err"; status_file="$dir/$base$suffix.exit"
scheduler=local
if [[ $queue != terminal && $queue != none ]]; then
  if command -v sbatch >/dev/null; then scheduler=slurm
  elif command -v qsub >/dev/null; then scheduler=pbs
  else echo 'Run aborted: no scheduler found; use -q terminal for local execution.' >&2; exit 1; fi
fi
script=$(mktemp "${TMPDIR:-/tmp}/qcfast.XXXXXX")
trap 'rm -f "$script"' EXIT
{
  echo '#!/usr/bin/env bash'
  if [[ $scheduler == slurm ]]; then
    printf '#SBATCH --job-name=%s%s\n#SBATCH --output="%s"\n#SBATCH --error="%s"\n#SBATCH --partition=%s\n#SBATCH --ntasks=1\n#SBATCH --cpus-per-task=1\n' "$job_name" "$suffix" "$log" "$err" "$queue"
    [[ -z ${SLURM_MEM:-} ]] || printf '#SBATCH --mem=%s\n' "$SLURM_MEM"
    [[ -z ${SLURM_MEMPERCPU:-} ]] || printf '#SBATCH --mem-per-cpu=%s\n' "$SLURM_MEMPERCPU"
  elif [[ $scheduler == pbs ]]; then
    printf '#PBS -N %s%s\n#PBS -o "%s"\n#PBS -e "%s"\n#PBS -l nodes=1:ppn=1\n' "$job_name" "$suffix" "$log" "$err"
  fi
  printf 'status_file=%q\n' "$status_file"
  echo 'trap '\''rc=$?; printf "%s\n" "$rc" > "$status_file.tmp.$$"; mv "$status_file.tmp.$$" "$status_file"'\'' EXIT'
  printf 'cd %q || exit 1\n' "$dir"
  echo 'echo "Running on $(hostname)"; echo "Start time: $(date)"'
  [[ $ui == 0 ]] || echo 'export QT_QPA_PLATFORM=offscreen; export MPLBACKEND=Agg'
  printf '%q ' "${command[@]}"; echo
  echo 'exit $?'
} > "$script"
if [[ $preview == 1 ]]; then cat "$script"; exit 0; fi
rm -f "$status_file" "$log" "$err"
if [[ $ui == 0 ]]; then
  if [[ -n ${STOPFDSMAXITER:-} ]]; then printf '%s\n' "$STOPFDSMAXITER" > "$dir/$base.stop"
  else rm -f "$dir/$base.stop"; fi
fi
cp "$script" "$dir/$base$suffix.slog"
job_id=; rc=0
case "$scheduler:$queue" in
  local:terminal)
    bash "$dir/$base$suffix.slog" > "$log" 2> "$err" || rc=$?
    cat "$log"; cat "$err" >&2;;
  local:none)
    bash "$dir/$base$suffix.slog" > "$log" 2> "$err" &
    job_id=$!; echo "Started local job $job_id";;
  slurm:*)
    result=$(sbatch "$script") || { echo 'Run aborted: sbatch failed.' >&2; exit 1; }
    echo "$result"; job_id=$(awk '/Submitted batch job/ {print $4}' <<< "$result")
    [[ $job_id =~ ^[0-9]+$ ]] || { echo 'Run aborted: missing Slurm job ID.' >&2; exit 1; };;
  pbs:*)
    job_id=$(qsub -q "$queue" "$script") || { echo 'Run aborted: qsub failed.' >&2; exit 1; }
    [[ $job_id =~ ^[0-9]+([.][A-Za-z0-9._-]+)?$ ]] || { echo 'Run aborted: invalid PBS job ID.' >&2; exit 1; }
    echo "Submitted PBS job $job_id";;
esac
if [[ -n ${CFAST_JOB_MANIFEST:-} ]]; then
  printf '%s\t%s\t%s\n' "$scheduler" "${job_id:-0}" "$status_file" >> "$CFAST_JOB_MANIFEST"
fi
exit "$rc"

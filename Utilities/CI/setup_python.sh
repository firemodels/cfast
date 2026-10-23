#!/usr/bin/env bash
# Prefer the sibling FDS environment; provision CFAST's environment only as a fallback.
setup_cfast_python() {
  local root=$1
  if [[ -n ${CFAST_CI_PYTHON:-} ]]; then
    # Explicitly supplied, already provisioned interpreter (also used for local tests).
    export CFAST_PYTHON=$CFAST_CI_PYTHON
  elif [[ -x $root/fds/.github/fds_python_env/bin/python ]]; then
    export CFAST_PYTHON="$root/fds/.github/fds_python_env/bin/python"
  else
    source "$root/cfast/Utilities/Python/setup_python_env.sh" --batchmode || return 1
    export CFAST_PYTHON="$root/cfast/.github/cfast_python_env/bin/python"
  fi
  echo "CFAST Python: $CFAST_PYTHON"
  export PATH="$(dirname "$CFAST_PYTHON"):$PATH"
  export PYTHONPATH="$root/fds/Utilities/Python${PYTHONPATH:+:$PYTHONPATH}"
  export MPLCONFIGDIR="$root/cfast/.github/cfast_matplotlib"
  mkdir -p "$MPLCONFIGDIR" || return 1
  "$CFAST_PYTHON" -c 'import fdsplotlib, numpy, pandas, scipy, matplotlib, PySide6' || return 1
}

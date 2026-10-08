#!/usr/bin/env bash
# Source with the repository collection root. Keep the environment outside checkouts.
setup_cfast_python() {
  local root=$1 state=${CFAST_CI_STATE_DIR:-$HOME/.cfastbot} requirements venv
  requirements="$root/fds/.github/requirements.txt"
  venv="$state/python_env"
  if [[ -n ${CFAST_CI_PYTHON:-} ]]; then
    # Explicitly supplied, already provisioned interpreter (also used for local tests).
    export CFAST_PYTHON=$CFAST_CI_PYTHON
  else
    [[ -f $requirements ]] || { echo "Missing Python requirements: $requirements" >&2; return 1; }
    [[ -x $venv/bin/python ]] || python3 -m venv "$venv" || return 1
    if ! cmp -s "$requirements" "$venv/cfast_requirements.txt"; then
      (cd "$root/fds/.github" && "$venv/bin/python" -m pip install -r requirements.txt) || return 1
      cp "$requirements" "$venv/cfast_requirements.txt"
    fi
    export CFAST_PYTHON="$venv/bin/python"
  fi
  export PATH="$(dirname "$CFAST_PYTHON"):$PATH"
  export PYTHONPATH="$root/fds/Utilities/Python${PYTHONPATH:+:$PYTHONPATH}"
  export MPLCONFIGDIR="$state/matplotlib"
  mkdir -p "$MPLCONFIGDIR"
  "$CFAST_PYTHON" -c 'import fdsplotlib, numpy, pandas, scipy, matplotlib, PySide6' || return 1
}

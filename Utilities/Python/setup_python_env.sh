#!/usr/bin/env bash
# Configure and activate CFAST's environment, using the FDS package requirements.
# Usage (Linux/macOS): source ./setup_python_env.sh [--batchmode]
# Existing environments are reused; changed requirements are installed in place.

setup_cfast_python_env() {
  local arg repo requirements venv
  for arg in "$@"; do
    [[ $arg == --batchmode ]] || { echo "Unknown argument: $arg" >&2; return 1; }
  done
  repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd) || return 1
  requirements="$repo/.github/requirements.txt"
  venv="$repo/.github/cfast_python_env"
  [[ -f $requirements ]] || { echo "Missing Python requirements: $requirements" >&2; return 1; }
  if [[ ! -x $venv/bin/python ]]; then
    python3 -c 'import sys; sys.exit(sys.version_info < (3, 7))' || {
      echo 'Python 3.7 or higher is required (packages may require a newer version).' >&2; return 1;
    }
    python3 -m venv "$venv" || return 1
  fi
  if ! cmp -s "$requirements" "$venv/cfast_requirements.txt"; then
    "$venv/bin/python" -m pip install --upgrade pip || return 1
    (cd "$repo/.github" && "$venv/bin/python" -m pip install -r requirements.txt) || return 1
    cp "$requirements" "$venv/cfast_requirements.txt" || return 1
  fi
  source "$venv/bin/activate" || return 1
  export PYTHONPATH="$repo/../fds/Utilities/Python${PYTHONPATH:+:$PYTHONPATH}"
  "$venv/bin/python" -c 'import fdsplotlib, numpy, pandas, scipy, matplotlib, PySide6' || return 1
  echo "CFAST Python environment ready: $venv"
}

setup_cfast_python_env "$@"

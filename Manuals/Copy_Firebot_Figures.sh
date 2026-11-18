#!/bin/bash
set -e

REPO_ROOT=/home/firebot/FireModels_cbot/cfast
BASEDIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  echo "Usage: $0 [--repo-root CFAST_REPO_ROOT]"
  echo "Default source: $REPO_ROOT"
  echo "Copies SCRIPT_FIGURES into the manuals beside this script."
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    -r|--repo-root)
      if [ "$#" -lt 2 ] || [ -z "$2" ]; then
        echo "Error: --repo-root requires a directory." >&2
        exit 1
      fi
      REPO_ROOT=$2
      shift 2
      ;;
    -h|--help) usage; exit 0 ;;
    *) usage >&2; exit 1 ;;
  esac
done

if [ ! -d "$REPO_ROOT/Manuals" ]; then
  echo "Error: cannot access $REPO_ROOT/Manuals" >&2
  exit 1
fi

COPIED=0
for GUIDE in CFAST_Tech_Ref CFAST_Users_Guide CFAST_Validation_Guide CFAST_Configuration_Guide; do
  SOURCE="$REPO_ROOT/Manuals/$GUIDE/SCRIPT_FIGURES"
  DESTINATION="$BASEDIR/$GUIDE/SCRIPT_FIGURES"
  [ -d "$SOURCE" ] || continue
  if [ "$SOURCE" -ef "$DESTINATION" ]; then
    echo "Error: source and destination are the same: $SOURCE" >&2
    exit 1
  fi
  mkdir -p "$DESTINATION"
  cp -R "$SOURCE/." "$DESTINATION/"
  echo "$GUIDE figures copied."
  COPIED=1
done

if [ "$COPIED" -eq 0 ]; then
  echo "Error: no SCRIPT_FIGURES directories found in $REPO_ROOT/Manuals" >&2
  exit 1
fi

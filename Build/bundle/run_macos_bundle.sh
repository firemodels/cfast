#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$SCRIPT_DIR"
./build_macos_bundle.sh --manuals-from-release --upload --notarize --notary-profile CFAST_NOTARY "$@"

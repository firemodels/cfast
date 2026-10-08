#!/usr/bin/env bash
SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
exec "$SCRIPT_DIR/../../Utilities/CI/Run_CFAST_Cases.sh" --suite Verification "$@"

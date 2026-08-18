#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PREFIX="[macos-bundle $(date '+%Y-%m-%d %H:%M:%S %z')]"

# A LaunchAgent has a deliberately small environment.  In particular, do not
# replace SSH_AUTH_SOCK with `launchctl getenv SSH_AUTH_SOCK`: that value is
# normally unset, while launchd has already passed the GUI login-session agent
# socket to this process.  GitHub SSH must also be batch-mode so a missing
# credential fails clearly instead of hanging an unattended build.
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export GIT_TERMINAL_PROMPT=0
export GIT_SSH_COMMAND="${GIT_SSH_COMMAND:-ssh -o BatchMode=yes -o ConnectTimeout=30}"

if [[ -z "${GH_TOKEN:-}" && -r "$HOME/.config/gh/cron_token" ]]; then
  export GH_TOKEN="$(<"$HOME/.config/gh/cron_token")"
fi

lock_dir="$SCRIPT_DIR/.macos_bundle.lock"
if ! mkdir "$lock_dir" 2>/dev/null; then
  echo "$LOG_PREFIX another macOS bundle run already holds $lock_dir; exiting"
  exit 0
fi
cleanup() {
  status=$?
  rmdir "$lock_dir" 2>/dev/null || true
  echo "[macos-bundle $(date '+%Y-%m-%d %H:%M:%S %z')] finished with status $status"
}
trap cleanup EXIT

echo "$LOG_PREFIX starting (SSH_AUTH_SOCK=${SSH_AUTH_SOCK:-unset})"

if [[ -z "${CODESIGN_ID:-}" ]]; then
  echo "$LOG_PREFIX ERROR: CODESIGN_ID is not configured."
  exit 1
fi

codesign_identities="$(/usr/bin/security find-identity -v -p codesigning 2>&1 || true)"
if ! grep -Fq -- "$CODESIGN_ID" <<<"$codesign_identities"; then
  echo "$LOG_PREFIX ERROR: the configured signing identity is unavailable: $CODESIGN_ID"
  echo "$codesign_identities"
  exit 1
fi
echo "$LOG_PREFIX code-signing preflight passed"

if [[ -z "${GH_TOKEN:-}" ]]; then
  echo "$LOG_PREFIX ERROR: GH_TOKEN is unset and ~/.config/gh/cron_token could not be read."
  exit 1
fi

# Force the login-session agent to load keys stored in the macOS Keychain.
# This is harmless when the key is already loaded and lets an unattended run
# recover after the agent has been restarted.
if [[ -n "${SSH_AUTH_SOCK:-}" ]]; then
  /usr/bin/ssh-add --apple-load-keychain >/dev/null 2>&1 || true
fi

github_probe() {
  git ls-remote --exit-code git@github.com:firemodels/cfast.git HEAD >/dev/null 2>&1
}

for attempt in 1 2 3; do
  if github_probe; then
    echo "$LOG_PREFIX GitHub SSH preflight passed"
    break
  fi
  if [[ "$attempt" == 3 ]]; then
    echo "$LOG_PREFIX ERROR: GitHub SSH preflight failed after $attempt attempts."
    echo "$LOG_PREFIX Ensure ~/.ssh/id_ed25519 is in the login Keychain and authorized for GitHub."
    exit 1
  fi
  echo "$LOG_PREFIX GitHub SSH preflight failed; retrying in 30 seconds ($attempt/3)"
  sleep 30
done

if [[ "${1:-}" == "--preflight-only" ]]; then
  echo "$LOG_PREFIX preflight-only run completed"
  exit 0
fi

cd "$SCRIPT_DIR"
./build_macos_bundle.sh --manuals-from-release --upload --notarize --notary-profile CFAST_NOTARY "$@"

#!/usr/bin/env bash
set -euo pipefail
FROMDIR=${1:?Manual directory required}
VERSIONDIR=${2:-${CFAST_CI_STATE_DIR:-$HOME/.cfastbot}/VERSION_LATEST}
: "${GH_OWNER:?Set GH_OWNER}" "${GH_REPO:?Set GH_REPO}" "${GH_CFAST_TAG:?Set GH_CFAST_TAG}"
for guide in CFAST_Tech_Ref CFAST_Users_Guide CFAST_Validation_Guide CFAST_Configuration_Guide; do
  file="$FROMDIR/$guide/$guide.pdf"
  [[ -f $file ]] || { echo "Missing guide: $file" >&2; exit 1; }
done
for key in CFAST_HASH SMV_HASH CFAST_REVISION SMV_REVISION; do
  [[ -s $VERSIONDIR/$key ]] || { echo "Missing version metadata: $VERSIONDIR/$key" >&2; exit 1; }
done
for guide in CFAST_Tech_Ref CFAST_Users_Guide CFAST_Validation_Guide CFAST_Configuration_Guide; do
  gh release upload "$GH_CFAST_TAG" "$FROMDIR/$guide/$guide.pdf" -R "$GH_OWNER/$GH_REPO" --clobber
done
info="$VERSIONDIR/CFAST_INFO.txt"
for key in CFAST_HASH SMV_HASH CFAST_REVISION SMV_REVISION; do
  printf '%s %s\n' "$key" "$(head -1 "$VERSIONDIR/$key")"
done > "$info"
gh release upload "$GH_CFAST_TAG" "$info" -R "$GH_OWNER/$GH_REPO" --clobber

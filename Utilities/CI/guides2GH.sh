#!/usr/bin/env bash
set -euo pipefail
FROMDIR=${1:?Manual directory required}
VERSIONDIR=${2:-${CFAST_CI_STATE_DIR:-$HOME/.cfastbot}/VERSION_LATEST}
: "${GH_OWNER:?Set GH_OWNER}" "${GH_REPO:?Set GH_REPO}" "${GH_CFAST_TAG:?Set GH_CFAST_TAG}"

upload_asset() {
  local file=$1 attempt=1 result output
  while :; do
    if output=$(gh release upload "$GH_CFAST_TAG" "$file" -R "$GH_OWNER/$GH_REPO" --clobber 2>&1); then
      [[ -z $output ]] || printf '%s\n' "$output"
      return 0
    else
      result=$?
    fi
    printf 'Upload failed for %s (attempt %s/3):\n%s\n' "$file" "$attempt" "$output" >&2
    # GitHub can fail while replacing an existing asset. Retry server errors
    # with --clobber so a partially completed replacement can be retried safely.
    if [[ $attempt == 3 || ! $output =~ HTTP[[:space:]]5[0-9][0-9] ]]; then
      return "$result"
    fi
    echo "Retrying upload in $((attempt * 10)) seconds." >&2
    sleep "$((attempt * 10))"
    attempt=$((attempt + 1))
  done
}

for guide in CFAST_Tech_Ref CFAST_Users_Guide CFAST_Validation_Guide CFAST_Configuration_Guide; do
  file="$FROMDIR/$guide/$guide.pdf"
  [[ -f $file ]] || { echo "Missing guide: $file" >&2; exit 1; }
done
for key in CFAST_HASH SMV_HASH CFAST_REVISION SMV_REVISION; do
  [[ -s $VERSIONDIR/$key ]] || { echo "Missing version metadata: $VERSIONDIR/$key" >&2; exit 1; }
done
for guide in CFAST_Tech_Ref CFAST_Users_Guide CFAST_Validation_Guide CFAST_Configuration_Guide; do
  upload_asset "$FROMDIR/$guide/$guide.pdf"
done
info="$VERSIONDIR/CFAST_INFO.txt"
for key in CFAST_HASH SMV_HASH CFAST_REVISION SMV_REVISION; do
  printf '%s %s\n' "$key" "$(head -1 "$VERSIONDIR/$key")"
done > "$info"
upload_asset "$info"

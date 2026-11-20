#!/bin/bash
set -eu
cd "$(dirname "$0")"
for manual in tech user validation config; do
  python3 preview/build_preview.py --manual "$manual" --check
done
for manual in tech user validation config; do
  python3 preview/build_preview.py --manual "$manual"
done
destination="$HOME/.cfastbot/online-docs"
mkdir -p "$destination"
cp build/index.html build/catalog.css build/catalog.js build/manuals.json "$destination/"
for manual in tech-guide user-guide validation-guide config-guide; do
  mkdir -p "$destination/$manual"
  cp -R "build/$manual/html" "$destination/$manual/"
done
echo "Online manuals ready in $destination"

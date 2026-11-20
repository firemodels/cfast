#!/bin/bash
set -eu
cd "$(dirname "$0")"
for manual in tech user validation config; do
  python3 preview/build_preview.py --manual "$manual" --check
done
for manual in tech user validation config; do
  python3 preview/build_preview.py --manual "$manual"
done
destination="${CFAST_CI_STATE_DIR:-$HOME/.cfastbot}/online-docs"
mkdir -p "$destination"
cp build/index.html build/catalog.css build/manuals.json "$destination/"
for manual in tech-guide user-guide validation-guide config-guide; do
  mkdir -p "$destination/$manual"
  cp -R "build/$manual/html" "$destination/$manual/"
done
echo "Online manuals ready in $destination"
python3 - "$destination.zip" <<'PY'
from pathlib import Path
import sys
from zipfile import ZipFile, ZIP_DEFLATED

root = Path('build')
files = [root / name for name in ('index.html', 'catalog.css', 'manuals.json')]
for manual in ('tech-guide', 'user-guide', 'validation-guide', 'config-guide'):
    files.extend(p for p in (root / manual / 'html').rglob('*') if p.is_file())
with ZipFile(sys.argv[1], 'w', compression=ZIP_DEFLATED) as archive:
    for path in files:
        archive.write(path, path.relative_to(root))
print(f'Online manuals packaged in {sys.argv[1]}')
PY

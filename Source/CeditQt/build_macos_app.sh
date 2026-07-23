#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

APP_NAME="CFAST Editor (CEdit)"
BUNDLE_IDENTIFIER="gov.nist.firemodels.cedit"
ASSET_DIR="$SCRIPT_DIR/assets"
PYTHON_EXE="${PYTHON:-python3}"
OUT_DIR="$REPO_ROOT/Build/CeditQt/macos"
WORK_DIR="$REPO_ROOT/Build/CeditQt/pyinstaller-work"
SPEC_DIR="$REPO_ROOT/Build/CeditQt/spec"
PYINSTALLER_CACHE_DIR="$REPO_ROOT/Build/CeditQt/pyinstaller-cache"
ICON_PATH=""
CLEAN=1
AD_HOC_SIGN=0

usage()
{
  echo "Usage: build_macos_app.sh [options]"
  echo ""
  echo "Builds CEditQt as a macOS .app using PyInstaller."
  echo ""
  echo "Options:"
  echo "  --python path       Python executable to use (default: python3 or \$PYTHON)"
  echo "  --output-dir path   Directory for the .app (default: Build/CeditQt/macos)"
  echo "  --name name         Application name (default: CFAST Editor (CEdit))"
  echo "  --bundle-id id      macOS bundle identifier"
  echo "  --icon path         .icns or square .png icon file"
  echo "  --no-clean          Reuse the previous PyInstaller work directory"
  echo "  --ad-hoc-sign       Run codesign --sign - on the generated app"
  echo "  -h, --help          Display this message"
}

resolve_app_icon()
{
  local icon_ext

  if [[ "$ICON_PATH" == "" ]]; then
    if [[ -f "$ASSET_DIR/CeditQt.icns" ]]; then
      ICON_PATH="$ASSET_DIR/CeditQt.icns"
    elif [[ -f "$ASSET_DIR/CeditQt.png" ]]; then
      ICON_PATH="$ASSET_DIR/CeditQt.png"
    fi
  fi

  if [[ "$ICON_PATH" == "" ]]; then
    return 0
  fi

  if [[ ! -f "$ICON_PATH" ]]; then
    echo "***error: icon file not found: $ICON_PATH"
    exit 1
  fi

  icon_ext="$(printf "%s" "${ICON_PATH##*.}" | tr "[:upper:]" "[:lower:]")"
  if [[ "$icon_ext" != "icns" && "$icon_ext" != "png" ]]; then
    echo "***error: icon file must be .icns or .png: $ICON_PATH"
    exit 1
  fi

  APP_ICON="$ICON_PATH"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --python)
      PYTHON_EXE="$2"
      shift 2
      ;;
    --output-dir)
      OUT_DIR="$2"
      shift 2
      ;;
    --name)
      APP_NAME="$2"
      shift 2
      ;;
    --bundle-id)
      BUNDLE_IDENTIFIER="$2"
      shift 2
      ;;
    --icon)
      ICON_PATH="$2"
      shift 2
      ;;
    --no-clean)
      CLEAN=0
      shift
      ;;
    --ad-hoc-sign)
      AD_HOC_SIGN=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "***error: unknown option: $1"
      usage
      exit 1
      ;;
  esac
done

if [[ "$(uname)" != "Darwin" ]]; then
  echo "***error: CEditQt .app builds must be run on macOS."
  exit 1
fi

if ! command -v "$PYTHON_EXE" >/dev/null 2>&1; then
  echo "***error: Python executable not found: $PYTHON_EXE"
  exit 1
fi

if ! "$PYTHON_EXE" - <<'PY' >/dev/null 2>&1
import PyInstaller
import PySide6
import matplotlib
PY
then
  echo "***error: the selected Python environment must provide PyInstaller, PySide6, and matplotlib."
  echo "         Try: $PYTHON_EXE -m pip install pyinstaller"
  exit 1
fi

mkdir -p "$OUT_DIR" "$WORK_DIR" "$SPEC_DIR" "$PYINSTALLER_CACHE_DIR"
rm -rf "$OUT_DIR/$APP_NAME.app"

export PYINSTALLER_CONFIG_DIR="$PYINSTALLER_CACHE_DIR"
export MPLCONFIGDIR="${TMPDIR:-/tmp}/cedit-qt-matplotlib"
export MPLBACKEND=QtAgg
export QT_API=PySide6
mkdir -p "$MPLCONFIGDIR"

APP_ICON=""
resolve_app_icon

pyinstaller_args=(
  --noconfirm
  --windowed
  --name "$APP_NAME"
  --distpath "$OUT_DIR"
  --workpath "$WORK_DIR"
  --specpath "$SPEC_DIR"
  --osx-bundle-identifier "$BUNDLE_IDENTIFIER"
  --hidden-import matplotlib.backends.backend_qtagg
  --hidden-import matplotlib.backends.backend_qt
  --hidden-import matplotlib.backends.qt_compat
  --exclude-module PyQt5
  --exclude-module PyQt6
  --exclude-module PySide2
)

if [[ "$APP_ICON" != "" ]]; then
  pyinstaller_args+=(--icon "$APP_ICON")
fi

if [[ "$CLEAN" == "1" ]]; then
  pyinstaller_args+=(--clean)
fi

echo "*** Building $APP_NAME.app"
echo "*** Python: $("$PYTHON_EXE" -c 'import sys; print(sys.executable)')"
echo "*** Output: $OUT_DIR"
if [[ "$APP_ICON" != "" ]]; then
  echo "*** Icon: $APP_ICON"
fi

cd "$SCRIPT_DIR"
"$PYTHON_EXE" -m PyInstaller "${pyinstaller_args[@]}" cedit_qt.py

APP_PATH="$OUT_DIR/$APP_NAME.app"
APP_EXECUTABLE="$APP_PATH/Contents/MacOS/$APP_NAME"

if [[ ! -x "$APP_EXECUTABLE" ]]; then
  echo "***error: expected app executable was not created: $APP_EXECUTABLE"
  exit 1
fi

if [[ "$AD_HOC_SIGN" == "1" ]]; then
  if ! command -v codesign >/dev/null 2>&1; then
    echo "***error: codesign not found."
    exit 1
  fi
  echo "*** Ad-hoc signing $APP_PATH"
  codesign --force --deep --sign - "$APP_PATH"
fi

echo "*** CEditQt app built:"
echo "    $APP_PATH"

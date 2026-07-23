#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
FIREMODELS_ROOT="$(cd "$REPO_ROOT/.." && pwd)"

APP_NAME="CFAST Editor (CEdit)"
DIST_NAME=""
VOLUME_NAME=""
OUTPUT_DIR="$REPO_ROOT/Build/bundle/macos"
STAGE_ROOT="$REPO_ROOT/Build/bundle/stage"
CFAST_EXE="$REPO_ROOT/Build/CFAST/gnu_osx/cfast7_osx"
CEDIT_APP="$REPO_ROOT/Build/CeditQt/macos/$APP_NAME.app"
EXAMPLE_FILE="$REPO_ROOT/Utilities/for_bundle/Bin/Data/Users_Guide_Example.in"
SMV_EXE="$FIREMODELS_ROOT/smv/Build/smokeview/gnu_osx/smokeview_osx"
SMV_BUNDLE_DIR="$FIREMODELS_ROOT/smv/Build/for_bundle"
INCLUDE_SMOKEVIEW=1
CREATE_DMG=1
CUSTOMIZE_DMG=1

usage()
{
  echo "Usage: build_macos_bundle.sh [options]"
  echo ""
  echo "Stages a CFAST macOS bundle and creates a DMG with dmgbuild."
  echo ""
  echo "Options:"
  echo "  --name name              Distribution folder name"
  echo "  --volume-name name       DMG volume name"
  echo "  --output-dir path        Output directory for the DMG"
  echo "  --stage-dir path         Temporary staging directory"
  echo "  --cfast-exe path         CFAST executable to bundle"
  echo "  --cedit-app path         CEditQt .app to bundle"
  echo "  --example path           Example .in file to bundle"
  echo "  --smokeview-exe path     Smokeview executable to bundle"
  echo "  --smokeview-data path    Smokeview for_bundle directory"
  echo "  --no-smokeview           Do not bundle Smokeview files"
  echo "  --no-dmg                 Stage files only"
  echo "  --no-layout              Do not use a DMG background image"
  echo "  -h, --help               Display this message"
}

require_file()
{
  local file_path="$1"
  local description="$2"

  if [[ ! -f "$file_path" ]]; then
    echo "***error: $description not found: $file_path"
    exit 1
  fi
}

require_dir()
{
  local dir_path="$1"
  local description="$2"

  if [[ ! -d "$dir_path" ]]; then
    echo "***error: $description not found: $dir_path"
    exit 1
  fi
}

copy_file()
{
  local from_path="$1"
  local to_path="$2"

  mkdir -p "$(dirname "$to_path")"
  cp -p "$from_path" "$to_path"
}

copy_dir()
{
  local from_path="$1"
  local to_path="$2"

  mkdir -p "$(dirname "$to_path")"
  ditto "$from_path" "$to_path"
}

copy_optional_file()
{
  local from_path="$1"
  local to_path="$2"

  if [[ -f "$from_path" ]]; then
    copy_file "$from_path" "$to_path"
  fi
}

copy_optional_dir()
{
  local from_path="$1"
  local to_path="$2"

  if [[ -d "$from_path" ]]; then
    copy_dir "$from_path" "$to_path"
  fi
}

create_background()
{
  local out_file="$1"

  mkdir -p "$(dirname "$out_file")"

  if ! command -v python3 >/dev/null 2>&1; then
    echo "***error: python3 not found; cannot create DMG background."
    exit 1
  fi

  python3 - "$out_file" <<'PY'
from pathlib import Path
import sys

try:
    from PIL import Image, ImageDraw, ImageFont
except Exception as exc:
    raise SystemExit(f"***error: Pillow is required to create the DMG background: {exc}")

def load_font(size, bold=False):
    candidates = [
        "/System/Library/Fonts/Supplemental/Arial Bold.ttf" if bold else "/System/Library/Fonts/Supplemental/Arial.ttf",
        "/System/Library/Fonts/Helvetica.ttc",
        "/System/Library/Fonts/Supplemental/Helvetica.ttf",
    ]
    for candidate in candidates:
        try:
            return ImageFont.truetype(candidate, size)
        except Exception:
            pass
    return ImageFont.load_default()

def s(value, scale):
    return int(round(value * scale))

def box(values, scale):
    return tuple(s(value, scale) for value in values)

def draw_background(path, scale):
    oversample = 4
    render_scale = scale * oversample
    width, height = 720 * render_scale, 420 * render_scale
    image = Image.new("RGB", (width, height), (247, 249, 252))
    draw = ImageDraw.Draw(image)

    title_font = load_font(30 * render_scale, bold=True)
    caption_font = load_font(16 * render_scale)

    title = "Drag CFAST to Applications"
    caption = "Copy the CFAST folder to install CFAST 8."

    title_box = draw.textbbox((0, 0), title, font=title_font)
    title_x = (width - (title_box[2] - title_box[0])) // 2
    draw.text((title_x, s(54, render_scale)), title, fill=(35, 44, 58), font=title_font)

    caption_box = draw.textbbox((0, 0), caption, font=caption_font)
    caption_x = (width - (caption_box[2] - caption_box[0])) // 2
    draw.text((caption_x, s(94, render_scale)), caption, fill=(86, 100, 120), font=caption_font)

    arrow_color = (0, 122, 255)
    draw.rounded_rectangle(
        box((275, 201, 445, 209), render_scale),
        radius=s(4, render_scale),
        fill=arrow_color,
    )
    draw.polygon(
        [
            (s(455, render_scale), s(205, render_scale)),
            (s(420, render_scale), s(180, render_scale)),
            (s(420, render_scale), s(230, render_scale)),
        ],
        fill=arrow_color,
    )

    outline = (216, 223, 232)
    draw.rounded_rectangle(
        box((92, 145, 242, 284), render_scale),
        radius=s(18, render_scale),
        outline=outline,
        width=s(2, render_scale),
    )
    draw.rounded_rectangle(
        box((478, 145, 628, 284), render_scale),
        radius=s(18, render_scale),
        outline=outline,
        width=s(2, render_scale),
    )

    if oversample > 1:
        image = image.resize((720 * scale, 420 * scale), Image.Resampling.LANCZOS)

    image.save(path)

out_file = Path(sys.argv[1])
for scale in (1, 2, 3, 4):
    if scale == 1:
        scale_file = out_file
    else:
        scale_file = out_file.with_name(f"{out_file.stem}@{scale}x{out_file.suffix}")
    draw_background(scale_file, scale)
PY
}

detach_volume()
{
  local volume_name="$1"
  local volume_path="/Volumes/$volume_name"

  if mount | grep -q " on $volume_path "; then
    hdiutil detach "$volume_path" >/dev/null
  fi
}

write_dmgbuild_settings()
{
  local settings_file="$1"
  local background_file="$2"

  SETTINGS_FILE="$settings_file" \
  DIST_DIR="$DIST_DIR" \
  BACKGROUND_FILE="$background_file" \
  CUSTOMIZE_DMG="$CUSTOMIZE_DMG" \
  python3 - <<'PY'
import os
from pathlib import Path

settings_file = Path(os.environ["SETTINGS_FILE"])
dist_dir = os.environ["DIST_DIR"]
background_file = os.environ["BACKGROUND_FILE"]
customize_dmg = os.environ["CUSTOMIZE_DMG"] == "1"

background_line = f"background = {background_file!r}\n" if customize_dmg else "background = None\n"
background_hide = ""
background_location = ""
if customize_dmg:
    background_hide = """\
hide = [
    ".background.tiff",
]

"""
    background_location = '    ".background.tiff": (1000, 1000),\n'

settings_file.write_text(
    f"""\
format = "UDZO"
filesystem = "HFS+"
compression_level = 9
size = None

files = [
    ({dist_dir!r}, "CFAST"),
]

symlinks = {{
    "Applications": "/Applications",
}}

{background_hide}{background_line}window_rect = ((120, 120), (720, 420))
default_view = "icon-view"
show_status_bar = False
show_tab_view = False
show_toolbar = False
show_pathbar = False
show_sidebar = False
arrange_by = None
icon_size = 96
text_size = 16
icon_locations = {{
    "CFAST": (170, 235),
    "Applications": (550, 235),
{background_location}}}
""",
    encoding="utf-8",
)
PY
}

create_compressed_dmg()
{
  local settings_file="$STAGE_ROOT/dmgbuild_settings.py"
  local background_file="$STAGE_ROOT/dmg_background.png"

  detach_volume "$VOLUME_NAME" || true

  if [[ "$CUSTOMIZE_DMG" == "1" ]]; then
    create_background "$background_file"
  fi
  write_dmgbuild_settings "$settings_file" "$background_file"

  echo "*** Creating DMG with dmgbuild"
  if ! python3 -m dmgbuild \
    --settings "$settings_file" \
    --detach-retries 10 \
    "$VOLUME_NAME" \
    "$DMG_PATH"; then
    return 1
  fi
}

write_cfast_vars()
{
  local out_file="$1"

  cat > "$out_file" <<'EOF'
# Source this file from bash or zsh to add CFAST and Smokeview to PATH.

if [ -n "${BASH_VERSION:-}" ] && [ -n "${BASH_SOURCE:-}" ]; then
    _cfast_vars_file="${BASH_SOURCE[0]}"
elif [ -n "${ZSH_VERSION:-}" ]; then
    eval '_cfast_vars_file="${(%):-%x}"'
else
    _cfast_vars_file="$0"
fi

_cfast_vars_dir="$(CDPATH= cd -- "$(dirname -- "$_cfast_vars_file")" 2>/dev/null && pwd -P)"

if [ -n "$_cfast_vars_dir" ]; then
    CFAST_HOME="$(CDPATH= cd -- "$_cfast_vars_dir/.." 2>/dev/null && pwd -P)"
    export CFAST_HOME

    case ":${PATH:-}:" in
        *":$CFAST_HOME/bin:"*) ;;
        *) export PATH="$CFAST_HOME/bin:${PATH:-}" ;;
    esac

    if [ -d "$CFAST_HOME/SMV6" ]; then
        case ":${PATH:-}:" in
            *":$CFAST_HOME/SMV6:"*) ;;
            *) export PATH="$CFAST_HOME/SMV6:${PATH:-}" ;;
        esac
    fi
fi

unset _cfast_vars_file _cfast_vars_dir
EOF

  chmod +x "$out_file"
}

write_readme()
{
  local out_file="$1"

  cat > "$out_file" <<'EOF'
CFAST macOS Bundle
===================

This bundle contains:

- CFAST Editor (CEdit).app
- bin/cfast and bin/cfast7_osx
- Documentation/*.pdf
- Examples/Users_Guide_Example.in
- SMV6/smokeview, if Smokeview was available when the bundle was made

To install, drag the CFAST folder to Applications.

To use CFAST from a terminal, source:

    source "/Applications/CFAST/bin/CFASTVARS.sh"

Then run:

    cfast /Applications/CFAST/Examples/Users_Guide_Example.in

EOF
}

sanitize_name()
{
  printf "%s" "$1" | tr -cs "[:alnum:]_.-" "-"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --name)
      DIST_NAME="$2"
      shift 2
      ;;
    --volume-name)
      VOLUME_NAME="$2"
      shift 2
      ;;
    --output-dir)
      OUTPUT_DIR="$2"
      shift 2
      ;;
    --stage-dir)
      STAGE_ROOT="$2"
      shift 2
      ;;
    --cfast-exe)
      CFAST_EXE="$2"
      shift 2
      ;;
    --cedit-app)
      CEDIT_APP="$2"
      shift 2
      ;;
    --example)
      EXAMPLE_FILE="$2"
      shift 2
      ;;
    --smokeview-exe)
      SMV_EXE="$2"
      shift 2
      ;;
    --smokeview-data)
      SMV_BUNDLE_DIR="$2"
      shift 2
      ;;
    --no-smokeview)
      INCLUDE_SMOKEVIEW=0
      shift
      ;;
    --no-dmg)
      CREATE_DMG=0
      shift
      ;;
    --no-layout)
      CUSTOMIZE_DMG=0
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
  echo "***error: macOS bundles must be built on macOS."
  exit 1
fi

if [[ "$DIST_NAME" == "" ]]; then
  if git -C "$REPO_ROOT" describe --tags --dirty --always >/dev/null 2>&1; then
    cfast_version="$(git -C "$REPO_ROOT" describe --tags --dirty --always)"
    if [[ "$cfast_version" == CFAST* ]]; then
      DIST_NAME="$cfast_version-macos"
    else
      DIST_NAME="CFAST-$cfast_version-macos"
    fi
  else
    DIST_NAME="CFAST-macos"
  fi
fi

if [[ "$VOLUME_NAME" == "" ]]; then
  VOLUME_NAME="$DIST_NAME"
fi

require_file "$CFAST_EXE" "CFAST executable"
require_dir "$CEDIT_APP" "CEditQt app"
require_file "$EXAMPLE_FILE" "CFAST example file"

mkdir -p "$OUTPUT_DIR"

DIST_DIR="$STAGE_ROOT/$DIST_NAME/CFAST"
DMG_NAME="$(sanitize_name "$DIST_NAME").dmg"
DMG_PATH="$OUTPUT_DIR/$DMG_NAME"

echo "*** Staging CFAST macOS bundle"
echo "*** Distribution: $DIST_NAME"
echo "*** Stage: $DIST_DIR"
echo "*** Output: $DMG_PATH"

rm -rf "$STAGE_ROOT"
mkdir -p "$DIST_DIR/bin" "$DIST_DIR/Documentation" "$DIST_DIR/Examples"
ln -s /Applications "$STAGE_ROOT/$DIST_NAME/Applications"

copy_dir "$CEDIT_APP" "$DIST_DIR/$APP_NAME.app"

copy_file "$CFAST_EXE" "$DIST_DIR/bin/cfast7_osx"
chmod +x "$DIST_DIR/bin/cfast7_osx"
ln -s cfast7_osx "$DIST_DIR/bin/cfast"

copy_file "$EXAMPLE_FILE" "$DIST_DIR/Examples/Users_Guide_Example.in"

copy_file "$REPO_ROOT/Manuals/CFAST_Configuration_Guide/CFAST_Configuration_Guide.pdf" "$DIST_DIR/Documentation/CFAST_Configuration_Guide.pdf"
copy_file "$REPO_ROOT/Manuals/CFAST_Tech_Ref/CFAST_Tech_Ref.pdf" "$DIST_DIR/Documentation/CFAST_Tech_Ref.pdf"
copy_file "$REPO_ROOT/Manuals/CFAST_Users_Guide/CFAST_Users_Guide.pdf" "$DIST_DIR/Documentation/CFAST_Users_Guide.pdf"
copy_file "$REPO_ROOT/Manuals/CFAST_Validation_Guide/CFAST_Validation_Guide.pdf" "$DIST_DIR/Documentation/CFAST_Validation_Guide.pdf"

write_cfast_vars "$DIST_DIR/bin/CFASTVARS.sh"
write_readme "$DIST_DIR/README.txt"

if [[ "$INCLUDE_SMOKEVIEW" == "1" ]]; then
  if [[ -f "$SMV_EXE" && -d "$SMV_BUNDLE_DIR" ]]; then
    echo "*** Adding Smokeview"
    mkdir -p "$DIST_DIR/SMV6"
    copy_file "$SMV_EXE" "$DIST_DIR/SMV6/smokeview"
    chmod +x "$DIST_DIR/SMV6/smokeview"
    copy_optional_file "$SMV_BUNDLE_DIR/objects.svo" "$DIST_DIR/SMV6/objects.svo"
    copy_optional_file "$SMV_BUNDLE_DIR/volrender.ssf" "$DIST_DIR/SMV6/volrender.ssf"
    copy_optional_file "$SMV_BUNDLE_DIR/smokeview.ini" "$DIST_DIR/SMV6/smokeview.ini"
    copy_optional_dir "$SMV_BUNDLE_DIR/colorbars" "$DIST_DIR/SMV6/colorbars"
    copy_optional_dir "$SMV_BUNDLE_DIR/textures" "$DIST_DIR/SMV6/textures"
  else
    echo "*** Warning: Smokeview artifacts not found; continuing without Smokeview."
    echo "             smokeview: $SMV_EXE"
    echo "             data:      $SMV_BUNDLE_DIR"
  fi
fi

if [[ "$CREATE_DMG" == "1" ]]; then
  if ! python3 -c "import dmgbuild" >/dev/null 2>&1; then
    echo "***error: dmgbuild is not available in python3."
    exit 1
  fi

  if ! create_compressed_dmg; then
    echo "***error: macOS DMG creation failed."
    echo "         Confirm dmgbuild is installed in the active Python environment."
    exit 1
  fi

  echo "*** DMG created:"
  echo "    $DMG_PATH"
else
  echo "*** Bundle staged:"
  echo "    $STAGE_ROOT/$DIST_NAME"
fi

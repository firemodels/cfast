#!/usr/bin/env bash
set -euo pipefail

ORIGINAL_ARGS=("$@")
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
FIREMODELS_ROOT="$(cd "$REPO_ROOT/.." && pwd)"

APP_NAME="cedit"
DIST_NAME=""
VOLUME_NAME=""
OUTPUT_DIR="$REPO_ROOT/Build/bundle/macos"
STAGE_ROOT="$REPO_ROOT/Build/bundle/stage"
CFAST_BUILD_TARGET="gnu_macos"
CFAST_EXE="$REPO_ROOT/Build/CFAST/gnu_macos/cfast8_macos"
CFAST_EXE_SET=0
CEDIT_APP="$REPO_ROOT/Build/CeditQt/macos/$APP_NAME.app"
EXAMPLE_FILE="$REPO_ROOT/Utilities/for_bundle/Bin/Data/Users_Guide_Example.in"
SMV_EXE="$FIREMODELS_ROOT/smv/Build/smokeview/gnu_osx/smokeview_osx"
SMV_EXE_SET=0
SMV_BUNDLE_DIR="$FIREMODELS_ROOT/smv/Build/for_bundle"
SMV_BUILD_TARGET="gnu_osx"
PYTHON_EXE="${PYTHON:-python3}"
INCLUDE_CEDIT=1
INCLUDE_SMOKEVIEW=1
BUILD_CFAST=1
BUILD_CEDIT=1
BUILD_SMOKEVIEW=1
CREATE_DMG=1
CUSTOMIZE_DMG=1
RUNTIME_LIB_MANIFEST=""
UPDATE_REPOS=1
UPDATE_BRANCH="master"
MANUALS_FROM_RELEASE=0
MANUALS_RELEASE_REPO="firemodels/test_bundles"
MANUALS_RELEASE_TAG="CFAST_TEST"
MANUALS_DOWNLOAD_DIR="$STAGE_ROOT/release-manuals"
MANUALS_DOWNLOAD_DIR_SET=0
STRICT_REVISION=0
UPLOAD=0
if [[ -n "${GH_OWNER:-}" && -n "${GH_REPO:-}" ]]; then
  UPLOAD_RELEASE_REPO="$GH_OWNER/$GH_REPO"
else
  UPLOAD_RELEASE_REPO="firemodels/test_bundles"
fi
UPLOAD_RELEASE_TAG="${GH_CFAST_TAG:-CFAST_TEST}"

CONFIG_MANUAL="$REPO_ROOT/Manuals/CFAST_Configuration_Guide/CFAST_Configuration_Guide.pdf"
TECH_REF_MANUAL="$REPO_ROOT/Manuals/CFAST_Tech_Ref/CFAST_Tech_Ref.pdf"
USERS_GUIDE_MANUAL="$REPO_ROOT/Manuals/CFAST_Users_Guide/CFAST_Users_Guide.pdf"
VALIDATION_GUIDE_MANUAL="$REPO_ROOT/Manuals/CFAST_Validation_Guide/CFAST_Validation_Guide.pdf"
RELEASE_INFO_ASSET="CFAST_INFO.txt"
RELEASE_MANUAL_ASSETS=(
  "CFAST_Configuration_Guide.pdf"
  "CFAST_Tech_Ref.pdf"
  "CFAST_Users_Guide.pdf"
  "CFAST_Validation_Guide.pdf"
)

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
  echo "  --cfast-build-target target CFAST build target: gnu_macos or intel_macos"
  echo "  --cfast-exe path         CFAST executable to bundle"
  echo "  --cedit-app path         CEditQt .app to bundle"
  echo "  --example path           Example .in file to bundle"
  echo "  --smokeview-exe path     Smokeview executable to bundle"
  echo "  --smokeview-data path    Smokeview for_bundle directory"
  echo "  --smokeview-build-target target Smokeview build target: gnu_osx or clang_osx"
  echo "  --python path            Python executable used to build CEditQt and the DMG"
  echo "  --update-branch branch   Branch to update before building"
  echo "  --no-update-repos        Do not update the CFAST, Smokeview, and FDS repos"
  echo "  --no-build-cfast         Do not build CFAST before bundling"
  echo "  --no-build-cedit         Do not build CEditQt before bundling"
  echo "  --no-build-smokeview     Do not build Smokeview before bundling"
  echo "  --no-cedit               Do not bundle CEditQt"
  echo "  --no-smokeview           Do not bundle Smokeview files"
  echo "  --manuals-from-release   Download manuals from a GitHub release"
  echo "  --manuals-release-repo repo GitHub owner/repo containing manual assets"
  echo "  --manuals-release-tag tag GitHub release tag containing manual assets"
  echo "  --manuals-download-dir path Temporary directory for downloaded manuals"
  echo "  --strict-revision [bool] Require downloaded manuals to match local CFAST hash"
  echo "  --upload                 Upload the DMG to a GitHub release"
  echo "  --upload-release-repo repo GitHub owner/repo receiving the DMG"
  echo "  --upload-release-tag tag GitHub release tag receiving the DMG"
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

require_command()
{
  local command_name="$1"

  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "***error: required command not found: $command_name"
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

parse_bool_arg()
{
  local value

  value="$(printf "%s" "$1" | tr "[:upper:]" "[:lower:]")"
  case "$value" in
    1|true|yes|on)
      printf "1\n"
      ;;
    0|false|no|off)
      printf "0\n"
      ;;
    *)
      echo "***error: expected true or false, got: $1" >&2
      exit 1
      ;;
  esac
}

run_checked()
{
  local description="$1"
  shift

  if ! "$@"; then
    echo "***error: $description failed."
    exit 1
  fi
}

set_release_manual_sources()
{
  CONFIG_MANUAL="$MANUALS_DOWNLOAD_DIR/CFAST_Configuration_Guide.pdf"
  TECH_REF_MANUAL="$MANUALS_DOWNLOAD_DIR/CFAST_Tech_Ref.pdf"
  USERS_GUIDE_MANUAL="$MANUALS_DOWNLOAD_DIR/CFAST_Users_Guide.pdf"
  VALIDATION_GUIDE_MANUAL="$MANUALS_DOWNLOAD_DIR/CFAST_Validation_Guide.pdf"
}

release_asset_available()
{
  local assets="$1"
  local asset_name="$2"

  printf "%s\n" "$assets" | grep -Fxq "$asset_name"
}

current_git_hash()
{
  git -C "$REPO_ROOT" rev-parse HEAD 2>/dev/null || true
}

check_release_revision()
{
  local info_file="$1"
  local release_hash
  local local_hash

  release_hash="$(awk '$1 == "CFAST_HASH" {print $2; exit}' "$info_file")"
  local_hash="$(current_git_hash)"

  if [[ "$release_hash" == "" ]]; then
    echo "***error: CFAST_INFO.txt does not contain CFAST_HASH: $info_file"
    exit 1
  fi
  if [[ "$local_hash" == "" ]]; then
    echo "***error: unable to determine local CFAST git hash"
    exit 1
  fi
  if [[ "${local_hash:0:${#release_hash}}" != "$release_hash" ]]; then
    echo "***error: release manuals were generated from a different CFAST revision."
    echo "         release CFAST_HASH: $release_hash"
    echo "         local CFAST_HASH:   ${local_hash:0:${#release_hash}}"
    echo "         Rerun after the Linux Cfastbot -U job has uploaded matching manuals."
    exit 1
  fi
}

download_release_manuals()
{
  local assets
  local download_command
  local asset_name

  require_command gh

  echo "*** Checking manual release assets"
  if ! assets="$(gh release view "$MANUALS_RELEASE_TAG" \
    -R "$MANUALS_RELEASE_REPO" \
    --json assets \
    --jq ".assets[].name")"; then
    echo "***error: unable to read GitHub release manual assets."
    echo "         release: $MANUALS_RELEASE_REPO $MANUALS_RELEASE_TAG"
    exit 1
  fi

  for asset_name in "${RELEASE_MANUAL_ASSETS[@]}"; do
    if ! release_asset_available "$assets" "$asset_name"; then
      echo "***error: release is missing required CFAST manual asset: $asset_name"
      exit 1
    fi
  done

  if [[ "$STRICT_REVISION" == "1" ]] && ! release_asset_available "$assets" "$RELEASE_INFO_ASSET"; then
    echo "***error: release is missing required revision asset: $RELEASE_INFO_ASSET"
    exit 1
  fi

  rm -rf "$MANUALS_DOWNLOAD_DIR"
  mkdir -p "$MANUALS_DOWNLOAD_DIR"

  echo "*** Downloading manual release assets"
  download_command=(
    gh release download "$MANUALS_RELEASE_TAG"
    -R "$MANUALS_RELEASE_REPO"
    --dir "$MANUALS_DOWNLOAD_DIR"
    --clobber
  )
  for asset_name in "${RELEASE_MANUAL_ASSETS[@]}"; do
    download_command+=(-p "$asset_name")
  done
  if release_asset_available "$assets" "$RELEASE_INFO_ASSET"; then
    download_command+=(-p "$RELEASE_INFO_ASSET")
  fi
  run_checked "GitHub release manual download" "${download_command[@]}"

  for asset_name in "${RELEASE_MANUAL_ASSETS[@]}"; do
    require_file "$MANUALS_DOWNLOAD_DIR/$asset_name" "release asset $asset_name"
  done

  if [[ "$STRICT_REVISION" == "1" ]]; then
    check_release_revision "$MANUALS_DOWNLOAD_DIR/$RELEASE_INFO_ASSET"
  fi

  set_release_manual_sources
}

resolve_manual_sources()
{
  if [[ "$MANUALS_FROM_RELEASE" == "1" ]]; then
    download_release_manuals
  fi
}

tracked_local_changes()
{
  local repo_name="$1"
  local repo_dir="$2"
  local status_output

  git -C "$repo_dir" update-index --refresh >/dev/null 2>&1 || true
  status_output="$(git -C "$repo_dir" status --short --untracked-files=no)"
  if [[ "$status_output" != "" ]]; then
    if [[ "${STRICT_REVISION:-0}" == "1" ]]; then
      echo "***error: $repo_name repo has tracked local changes; refusing to update before strict bundle build."
      echo "         repo: $repo_dir"
      echo "$status_output"
      exit 1
    fi
    echo "*** Warning: $repo_name repo has tracked local changes; skipping update for this repo."
    echo "         repo: $repo_dir"
    echo "$status_output"
    return 0
  fi
  return 1
}

remote_branch_exists()
{
  local repo_dir="$1"
  local remote_name="$2"
  local branch_name="$3"

  git -C "$repo_dir" show-ref --verify --quiet "refs/remotes/$remote_name/$branch_name"
}

update_git_repo()
{
  local repo_name="$1"
  local repo_dir="$2"

  require_dir "$repo_dir/.git" "$repo_name git repository"
  if tracked_local_changes "$repo_name" "$repo_dir"; then
    return 1
  fi

  echo "*** Updating $repo_name repo"
  echo "    branch: $UPDATE_BRANCH"
  echo "    repo:   $repo_dir"
  run_checked "$repo_name checkout $UPDATE_BRANCH" git -C "$repo_dir" checkout "$UPDATE_BRANCH"
  if tracked_local_changes "$repo_name" "$repo_dir"; then
    return 1
  fi
  run_checked "$repo_name remote update" git -C "$repo_dir" remote update

  if remote_branch_exists "$repo_dir" origin "$UPDATE_BRANCH"; then
    run_checked "$repo_name merge origin/$UPDATE_BRANCH" git -C "$repo_dir" merge --ff-only "origin/$UPDATE_BRANCH"
  fi
  if remote_branch_exists "$repo_dir" firemodels "$UPDATE_BRANCH"; then
    run_checked "$repo_name merge firemodels/$UPDATE_BRANCH" git -C "$repo_dir" merge --ff-only "firemodels/$UPDATE_BRANCH"
  fi

}

update_bundle_repos()
{
  local updated_repo=0

  if [[ "$UPDATE_REPOS" != "1" ]]; then
    return 0
  fi
  if [[ "${CFAST_MACOS_BUNDLE_REEXECUTED:-}" == "1" ]]; then
    return 0
  fi

  update_git_repo cfast "$REPO_ROOT" && updated_repo=1
  update_git_repo smv "$FIREMODELS_ROOT/smv" && updated_repo=1
  update_git_repo fds "$FIREMODELS_ROOT/fds" && updated_repo=1

  if [[ "$updated_repo" != "1" ]]; then
    return 0
  fi

  echo "*** Re-starting macOS bundle script after repo updates"
  export CFAST_MACOS_BUNDLE_REEXECUTED=1
  if [[ "${#ORIGINAL_ARGS[@]}" -gt 0 ]]; then
    exec "$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")" "${ORIGINAL_ARGS[@]}"
  else
    exec "$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
  fi
}

build_cfast_executable()
{
  local build_dir
  local make_script

  if [[ "$BUILD_CFAST" != "1" ]]; then
    return 0
  fi

  build_dir="$REPO_ROOT/Build/CFAST/$CFAST_BUILD_TARGET"
  make_script="$build_dir/make_cfast.sh"
  require_dir "$build_dir" "CFAST build directory for $CFAST_BUILD_TARGET"

  echo "*** Building CFAST macOS executable ($CFAST_BUILD_TARGET)"
  if [[ -f "$make_script" ]]; then
    (cd "$build_dir" && run_checked "CFAST $CFAST_BUILD_TARGET build" bash ./make_cfast.sh)
  else
    require_command make
    (cd "$build_dir" && run_checked "CFAST $CFAST_BUILD_TARGET build" make -f ../makefile "$CFAST_BUILD_TARGET")
  fi
}

build_cedit_app()
{
  local build_script
  local cedit_output_dir
  local cedit_name

  if [[ "$INCLUDE_CEDIT" != "1" || "$BUILD_CEDIT" != "1" ]]; then
    return 0
  fi

  build_script="$REPO_ROOT/Build/CeditQt/build_macos_app.sh"
  require_file "$build_script" "CEditQt macOS build script"
  require_command "$PYTHON_EXE"

  cedit_output_dir="$(dirname "$CEDIT_APP")"
  cedit_name="$(basename "$CEDIT_APP")"
  cedit_name="${cedit_name%.app}"

  echo "*** Building CEditQt macOS app"
  run_checked "CEditQt macOS app build" \
    bash "$build_script" \
      --python "$PYTHON_EXE" \
      --output-dir "$cedit_output_dir" \
      --name "$cedit_name"
}

build_smokeview_executable()
{
  local libs_dir
  local smokeview_dir

  if [[ "$INCLUDE_SMOKEVIEW" != "1" || "$BUILD_SMOKEVIEW" != "1" ]]; then
    return 0
  fi

  libs_dir="$FIREMODELS_ROOT/smv/Build/LIBS/$SMV_BUILD_TARGET"
  smokeview_dir="$FIREMODELS_ROOT/smv/Build/smokeview/$SMV_BUILD_TARGET"
  require_file "$libs_dir/make_LIBS.sh" "Smokeview library build script"
  require_file "$smokeview_dir/make_smokeview.sh" "Smokeview macOS build script"

  echo "*** Building Smokeview macOS libraries ($SMV_BUILD_TARGET)"
  (cd "$libs_dir" && run_checked "Smokeview $SMV_BUILD_TARGET libraries build" bash ./make_LIBS.sh)

  echo "*** Building Smokeview macOS executable ($SMV_BUILD_TARGET)"
  (cd "$smokeview_dir" && run_checked "Smokeview $SMV_BUILD_TARGET build" bash ./make_smokeview.sh)
}

resolve_macos_dependency()
{
  local dependency="$1"
  local loader_path="$2"
  local loader_dir
  local dependency_name
  local candidate

  case "$dependency" in
    /usr/lib/*|/System/Library/*)
      return 1
      ;;
  esac

  loader_dir="$(dirname "$loader_path")"
  dependency_name="$(basename "$dependency")"

  case "$dependency" in
    /*)
      candidate="$dependency"
      ;;
    @rpath/*|@loader_path/*)
      candidate="$loader_dir/$dependency_name"
      ;;
    @executable_path/*)
      candidate="$loader_dir/${dependency#@executable_path/}"
      ;;
    *)
      return 1
      ;;
  esac

  if [[ -f "$candidate" ]]; then
    printf "%s\n" "$candidate"
    return 0
  fi

  return 1
}

record_macos_runtime_dependency()
{
  local load_name="$1"
  local source_path="$2"
  local library_name="$3"
  local record="$load_name|$source_path|$library_name"

  if ! grep -Fqx "$record" "$RUNTIME_LIB_MANIFEST"; then
    printf "%s\n" "$record" >> "$RUNTIME_LIB_MANIFEST"
  fi
}

macos_runtime_reference()
{
  local load_name="$1"
  local reference_prefix="$2"
  local recorded_load_name
  local source_path
  local library_name

  while IFS="|" read -r recorded_load_name source_path library_name; do
    if [[ "$recorded_load_name" == "$load_name" ]]; then
      printf "%s/%s\n" "$reference_prefix" "$library_name"
      return 0
    fi
  done < "$RUNTIME_LIB_MANIFEST"

  return 1
}

copy_macos_runtime_libraries()
{
  local source_binary="$1"
  local lib_dir="$2"
  local queue_file="$STAGE_ROOT/macos_runtime_queue.txt"
  local seen_file="$STAGE_ROOT/macos_runtime_seen.txt"
  local queue_index=1
  local source_path
  local load_name
  local dependency_path
  local library_name
  local destination_path

  mkdir -p "$lib_dir"
  : > "$RUNTIME_LIB_MANIFEST"
  : > "$queue_file"
  : > "$seen_file"
  printf "%s\n" "$source_binary" >> "$queue_file"

  while source_path="$(sed -n "${queue_index}p" "$queue_file")" && [[ "$source_path" != "" ]]; do
    queue_index=$((queue_index + 1))
    if grep -Fqx "$source_path" "$seen_file"; then
      continue
    fi
    printf "%s\n" "$source_path" >> "$seen_file"

    while read -r load_name; do
      if dependency_path="$(resolve_macos_dependency "$load_name" "$source_path")"; then
        library_name="$(basename "$dependency_path")"
        destination_path="$lib_dir/$library_name"
        record_macos_runtime_dependency "$load_name" "$dependency_path" "$library_name"
        if [[ "$dependency_path" != "$source_binary" && ! -f "$destination_path" ]]; then
          echo "*** Copying macOS runtime library: $library_name"
          copy_file "$dependency_path" "$destination_path"
          chmod u+w "$destination_path"
        fi
        if [[ "$dependency_path" != "$source_binary" ]]; then
          printf "%s\n" "$dependency_path" >> "$queue_file"
        fi
      fi
    done < <(otool -L "$source_path" | awk 'NR > 1 {print $1}')
  done
}

patch_macos_runtime_references()
{
  local target_path="$1"
  local reference_prefix="$2"
  local skip_first_dependency="$3"
  local dependency_index=0
  local load_name
  local new_reference

  while read -r load_name; do
    dependency_index=$((dependency_index + 1))
    if [[ "$skip_first_dependency" == "1" && "$dependency_index" == "1" ]]; then
      continue
    fi
    if new_reference="$(macos_runtime_reference "$load_name" "$reference_prefix")"; then
      if [[ "$load_name" != "$new_reference" ]]; then
        install_name_tool -change "$load_name" "$new_reference" "$target_path"
      fi
    fi
  done < <(otool -L "$target_path" | awk 'NR > 1 {print $1}')
}

codesign_macos_runtime_file()
{
  local target_path="$1"

  if command -v codesign >/dev/null 2>&1; then
    codesign --force --sign - "$target_path" >/dev/null 2>&1 || {
      echo "*** Warning: ad hoc codesign failed for $target_path"
    }
  fi
}

bundle_macos_runtime_libraries()
{
  local source_binary="$1"
  local staged_binary="$2"
  local lib_dir="$3"
  local library_path
  local library_name

  if ! command -v install_name_tool >/dev/null 2>&1; then
    echo "***error: install_name_tool is required to make the macOS bundle portable."
    exit 1
  fi

  RUNTIME_LIB_MANIFEST="$STAGE_ROOT/macos_runtime_manifest.txt"
  copy_macos_runtime_libraries "$source_binary" "$lib_dir"

  if [[ ! -s "$RUNTIME_LIB_MANIFEST" ]]; then
    return 0
  fi

  echo "*** Patching macOS runtime library paths"
  chmod u+w "$staged_binary"
  patch_macos_runtime_references "$staged_binary" "@loader_path/../lib" 0

  for library_path in "$lib_dir"/*.dylib; do
    if [[ ! -f "$library_path" ]]; then
      continue
    fi
    library_name="$(basename "$library_path")"
    install_name_tool -id "@loader_path/$library_name" "$library_path"
    patch_macos_runtime_references "$library_path" "@loader_path" 1
    codesign_macos_runtime_file "$library_path"
  done

  codesign_macos_runtime_file "$staged_binary"
}

create_background()
{
  local out_file="$1"

  mkdir -p "$(dirname "$out_file")"

  if ! command -v "$PYTHON_EXE" >/dev/null 2>&1; then
    echo "***error: Python executable not found: $PYTHON_EXE"
    exit 1
  fi

  "$PYTHON_EXE" - "$out_file" <<'PY'
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

    title = "Install CFAST 8"
    caption = "Drag CFAST8 to Applications, or run the Terminal install menu."

    title_box = draw.textbbox((0, 0), title, font=title_font)
    title_x = (width - (title_box[2] - title_box[0])) // 2
    draw.text((title_x, s(54, render_scale)), title, fill=(35, 44, 58), font=title_font)

    caption_box = draw.textbbox((0, 0), caption, font=caption_font)
    caption_x = (width - (caption_box[2] - caption_box[0])) // 2
    draw.text((caption_x, s(94, render_scale)), caption, fill=(86, 100, 120), font=caption_font)

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
  DMG_README="$DMG_README" \
  INSTALL_COMMAND="$INSTALL_COMMAND" \
  BACKGROUND_FILE="$background_file" \
  CUSTOMIZE_DMG="$CUSTOMIZE_DMG" \
  "$PYTHON_EXE" - <<'PY'
import os
from pathlib import Path

settings_file = Path(os.environ["SETTINGS_FILE"])
dist_dir = os.environ["DIST_DIR"]
dmg_readme = os.environ["DMG_README"]
install_command = os.environ["INSTALL_COMMAND"]
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
    ({dist_dir!r}, "CFAST8"),
    ({dmg_readme!r}, "README.txt"),
    ({install_command!r}, "Terminal Install Menu.command"),
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
    "CFAST8": (105, 235),
    "Applications": (275, 235),
    "README.txt": (455, 235),
    "Terminal Install Menu.command": (625, 235),
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

  rm -f "$DMG_PATH"
  echo "*** Creating DMG with dmgbuild"
  if ! "$PYTHON_EXE" -m dmgbuild \
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

- bin/cfast and bin/cfast8_macos
- cedit.app, if CEditQt was available when the bundle was made
- Documentation/*.pdf
- Examples/Users_Guide_Example.in
- SMV6/smokeview, if Smokeview was available when the bundle was made

To install manually, drag or copy the CFAST8 folder from the DMG into:

    /Applications

This installs CFAST into:

    /Applications/CFAST8

Finder will ask before replacing an existing /Applications/CFAST8 folder.

For the menu-style installer, run Terminal Install Menu.command from the DMG.
Choose option 1 to install to /Applications/CFAST8, or option 2 to extract the
CFAST8 folder to a revision-named folder in Downloads. Choose option 3 to
extract it to a revision-named folder in another directory.

To use CFAST from a terminal, source:

    source "/Applications/CFAST8/bin/CFASTVARS.sh"

To add CFAST to future Terminal sessions, add that source line to the profile
file for your shell:

    bash: ~/.bash_profile
    zsh:  ~/.zprofile

Then run:

    cfast /Applications/CFAST8/Examples/Users_Guide_Example.in

EOF
}

write_dmg_readme()
{
  local out_file="$1"

  cat > "$out_file" <<'EOF'
CFAST macOS Bundle
===================

To install manually, drag or copy the CFAST8 folder from this DMG into:

    /Applications

This installs CFAST into:

    /Applications/CFAST8

Finder will ask before replacing an existing /Applications/CFAST8 folder.

For the menu-style installer, run Terminal Install Menu.command from this
DMG. Choose option 1 to install to /Applications/CFAST8, or option 2 to extract
the CFAST8 folder to a revision-named folder in Downloads. Choose option 3 to
extract it to a revision-named folder in another directory.

After copying, source:

    source "/Applications/CFAST8/bin/CFASTVARS.sh"

To add CFAST to future Terminal sessions, add that source line to the profile
file for your shell:

    bash: ~/.bash_profile
    zsh:  ~/.zprofile

Then run:

    cfast /Applications/CFAST8/Examples/Users_Guide_Example.in
EOF
}

write_install_command()
{
  local out_file="$1"
  local extract_name="$2"

  {
  cat <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" 2>/dev/null && pwd -P)"
SOURCE_DIR="$SCRIPT_DIR/CFAST8"
INSTALL_DIR="/Applications/CFAST8"
EOF
  printf "EXTRACT_NAME=%q\n" "$extract_name"
  cat <<'EOF'
EXTRACT_DIR="$HOME/Downloads/$EXTRACT_NAME"

finish()
{
  printf "\nPress Enter to close."
  IFS= read -r _ || true
}
trap finish EXIT

copy_cfast8()
{
  local destination="$1"
  shift

  if [[ ! -d "$SOURCE_DIR" ]]; then
    echo "***error: bundled CFAST8 directory was not found: $SOURCE_DIR"
    exit 1
  fi

  if [[ -e "$destination" ]]; then
    printf "%s already exists. Replace it? [y/N]: " "$destination"
    IFS= read -r answer
    case "$answer" in
      y|Y|yes|YES) ;;
      *)
        echo "Cancelled."
        exit 1
        ;;
    esac
  fi

  if [[ "$#" -gt 0 ]]; then
    "$@" mkdir -p "$(dirname "$destination")"
    if [[ -e "$destination" ]]; then
      "$@" rm -rf "$destination"
    fi
    "$@" ditto "$SOURCE_DIR" "$destination"
  else
    mkdir -p "$(dirname "$destination")"
    if [[ -e "$destination" ]]; then
      rm -rf "$destination"
    fi
    ditto "$SOURCE_DIR" "$destination"
  fi
}

expand_path()
{
  local input_path="$1"

  case "$input_path" in
    "~")
      printf "%s\n" "$HOME"
      ;;
    "~/"*)
      printf "%s/%s\n" "$HOME" "${input_path#\~/}"
      ;;
    *)
      printf "%s\n" "$input_path"
      ;;
  esac
}

print_profile_guidance()
{
  local cfast_root="$1"
  local cfast_dir_value="$cfast_root"
  local shell_name="${SHELL:-}"
  local profile_display="~/.bash_profile"

  if [[ "$cfast_root" == "$HOME/"* ]]; then
    cfast_dir_value="\$HOME/${cfast_root#$HOME/}"
  fi
  shell_name="${shell_name##*/}"
  if [[ "$shell_name" == "zsh" ]]; then
    profile_display="~/.zprofile"
  fi

  echo ""
  echo "This Terminal appears to use ${shell_name:-bash}."
  echo "To add CFAST to future Terminal sessions, add these lines to $profile_display:"
  echo "    CFAST_DIR=\"$cfast_dir_value\""
  echo '    source "$CFAST_DIR/bin/CFASTVARS.sh"'
  echo ""
  echo "Then open a new Terminal window, or run:"
  echo "    source $profile_display"
}

install_default()
{
  if ! copy_cfast8 "$INSTALL_DIR"; then
    echo ""
    echo "*** Warning: install to /Applications failed without administrator privileges."
    printf "Try again with sudo? [y/N]: "
    IFS= read -r answer
    case "$answer" in
      y|Y|yes|YES)
        sudo -v
        copy_cfast8 "$INSTALL_DIR" sudo
        ;;
      *)
        echo "Install cancelled."
        return 1
        ;;
    esac
  fi

  echo ""
  echo "CFAST installed to: $INSTALL_DIR"
  echo "To use CFAST from a terminal:"
  echo "    source \"$INSTALL_DIR/bin/CFASTVARS.sh\""
  echo "    cfast \"$INSTALL_DIR/Examples/Users_Guide_Example.in\""
  print_profile_guidance "$INSTALL_DIR"
}

extract_to_downloads()
{
  copy_cfast8 "$EXTRACT_DIR"
  echo ""
  echo "CFAST extracted to: $EXTRACT_DIR"
  echo "You may rename this folder and move it wherever you want."
  print_profile_guidance "$EXTRACT_DIR"
}

extract_to_custom()
{
  local parent_dir
  local custom_extract_dir

  echo ""
  printf "Parent directory (for example, ~/Desktop): "
  IFS= read -r parent_dir
  parent_dir="$(expand_path "$parent_dir")"

  if [[ -z "$parent_dir" ]]; then
    echo "***error: no parent directory was entered."
    exit 1
  fi

  if [[ -e "$parent_dir" && ! -d "$parent_dir" ]]; then
    echo "***error: parent path is not a directory: $parent_dir"
    exit 1
  fi

  custom_extract_dir="$parent_dir/$EXTRACT_NAME"
  copy_cfast8 "$custom_extract_dir"
  echo ""
  echo "CFAST extracted to: $custom_extract_dir"
  echo "You may rename this folder and move it wherever you want."
  print_profile_guidance "$custom_extract_dir"
}

echo "CFAST 8 macOS installer"
echo ""
echo "1) Install CFAST8 to /Applications/CFAST8"
echo "2) Extract CFAST8 to ~/Downloads/$EXTRACT_NAME"
echo "3) Extract CFAST8 to another directory"
echo "q) Quit"
echo ""
printf "Select an option [1]: "
IFS= read -r choice

case "$choice" in
  ""|1)
    install_default
    ;;
  2)
    extract_to_downloads
    ;;
  3)
    extract_to_custom
    ;;
  q|Q|quit|QUIT|exit|EXIT)
    echo "Cancelled."
    exit 1
    ;;
  *)
    echo "***error: unknown option: $choice"
    exit 1
    ;;
esac

trap - EXIT
finish
EOF
  } > "$out_file"

  chmod +x "$out_file"
}

sanitize_name()
{
  printf "%s" "$1" | tr -cs "[:alnum:]_.-" "-"
}

copy_manuals()
{
  require_file "$CONFIG_MANUAL" "CFAST manual CFAST_Configuration_Guide.pdf"
  require_file "$TECH_REF_MANUAL" "CFAST manual CFAST_Tech_Ref.pdf"
  require_file "$USERS_GUIDE_MANUAL" "CFAST manual CFAST_Users_Guide.pdf"
  require_file "$VALIDATION_GUIDE_MANUAL" "CFAST manual CFAST_Validation_Guide.pdf"

  copy_file "$CONFIG_MANUAL" "$DIST_DIR/Documentation/CFAST_Configuration_Guide.pdf"
  copy_file "$TECH_REF_MANUAL" "$DIST_DIR/Documentation/CFAST_Tech_Ref.pdf"
  copy_file "$USERS_GUIDE_MANUAL" "$DIST_DIR/Documentation/CFAST_Users_Guide.pdf"
  copy_file "$VALIDATION_GUIDE_MANUAL" "$DIST_DIR/Documentation/CFAST_Validation_Guide.pdf"
}

release_asset_names()
{
  gh release view "$UPLOAD_RELEASE_TAG" \
    -R "$UPLOAD_RELEASE_REPO" \
    --json assets \
    --jq ".assets[].name"
}

is_cfast_macos_bundle_asset()
{
  local asset_name="$1"
  local lower_name

  lower_name="$(printf "%s" "$asset_name" | tr "[:upper:]" "[:lower:]")"
  case "$lower_name" in
    cfast*.dmg)
      case "$lower_name" in
        *macos*|*osx*|*darwin*)
          return 0
          ;;
      esac
      ;;
  esac

  return 1
}

upload_macos_bundle()
{
  local assets
  local asset_name
  local removed_count=0

  if [[ "$UPLOAD" != "1" ]]; then
    return 0
  fi
  if [[ "$CREATE_DMG" != "1" ]]; then
    echo "***error: --upload requires DMG creation; remove --no-dmg."
    exit 1
  fi

  require_command gh
  require_file "$DMG_PATH" "macOS DMG"

  echo "*** Preparing macOS bundle upload"
  echo "    release: $UPLOAD_RELEASE_REPO $UPLOAD_RELEASE_TAG"
  echo "    asset:   $(basename "$DMG_PATH")"
  if ! assets="$(release_asset_names)"; then
    echo "***error: unable to read GitHub release assets before upload."
    echo "         release: $UPLOAD_RELEASE_REPO $UPLOAD_RELEASE_TAG"
    exit 1
  fi

  while IFS= read -r asset_name; do
    if [[ "$asset_name" == "" ]]; then
      continue
    fi
    if is_cfast_macos_bundle_asset "$asset_name"; then
      echo "*** Removing previous CFAST macOS bundle: $asset_name"
      run_checked "GitHub release asset removal for $asset_name" \
        gh release delete-asset "$UPLOAD_RELEASE_TAG" "$asset_name" \
          -R "$UPLOAD_RELEASE_REPO" \
          -y
      removed_count=$((removed_count + 1))
    fi
  done <<< "$assets"

  if [[ "$removed_count" == "0" ]]; then
    echo "*** No previous CFAST macOS bundle found on release"
  fi

  echo "*** Uploading new CFAST macOS bundle: $(basename "$DMG_PATH")"
  run_checked "GitHub release upload" \
    gh release upload "$UPLOAD_RELEASE_TAG" "$DMG_PATH" \
      --clobber \
      -R "$UPLOAD_RELEASE_REPO"
  echo "*** macOS bundle upload complete"
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
    --cfast-build-target)
      CFAST_BUILD_TARGET="$2"
      shift 2
      ;;
    --cfast-exe)
      CFAST_EXE="$2"
      CFAST_EXE_SET=1
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
      SMV_EXE_SET=1
      shift 2
      ;;
    --smokeview-data)
      SMV_BUNDLE_DIR="$2"
      shift 2
      ;;
    --smokeview-build-target)
      SMV_BUILD_TARGET="$2"
      shift 2
      ;;
    --python)
      PYTHON_EXE="$2"
      shift 2
      ;;
    --update-branch)
      UPDATE_BRANCH="$2"
      shift 2
      ;;
    --no-update-repos)
      UPDATE_REPOS=0
      shift
      ;;
    --no-build-cfast)
      BUILD_CFAST=0
      shift
      ;;
    --no-build-cedit)
      BUILD_CEDIT=0
      shift
      ;;
    --no-build-smokeview)
      BUILD_SMOKEVIEW=0
      shift
      ;;
    --no-cedit)
      INCLUDE_CEDIT=0
      shift
      ;;
    --no-smokeview)
      INCLUDE_SMOKEVIEW=0
      shift
      ;;
    --manuals-from-release)
      MANUALS_FROM_RELEASE=1
      shift
      ;;
    --manuals-release-repo)
      MANUALS_RELEASE_REPO="$2"
      shift 2
      ;;
    --manuals-release-tag)
      MANUALS_RELEASE_TAG="$2"
      shift 2
      ;;
    --manuals-download-dir)
      MANUALS_DOWNLOAD_DIR="$2"
      MANUALS_DOWNLOAD_DIR_SET=1
      shift 2
      ;;
    --strict-revision)
      STRICT_REVISION=1
      if [[ $# -gt 1 && "$2" != -* ]]; then
        STRICT_REVISION="$(parse_bool_arg "$2")"
        shift 2
      else
        shift
      fi
      ;;
    --upload)
      UPLOAD=1
      shift
      ;;
    --upload-release-repo)
      UPLOAD_RELEASE_REPO="$2"
      shift 2
      ;;
    --upload-release-tag)
      UPLOAD_RELEASE_TAG="$2"
      shift 2
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

case "$CFAST_BUILD_TARGET" in
  gnu_macos|intel_macos)
    ;;
  *)
    echo "***error: unsupported CFAST macOS build target: $CFAST_BUILD_TARGET"
    exit 1
    ;;
esac

case "$SMV_BUILD_TARGET" in
  gnu_osx|clang_osx)
    ;;
  *)
    echo "***error: unsupported Smokeview macOS build target: $SMV_BUILD_TARGET"
    exit 1
    ;;
esac

if [[ "$CFAST_EXE_SET" == "0" ]]; then
  CFAST_EXE="$REPO_ROOT/Build/CFAST/$CFAST_BUILD_TARGET/cfast8_macos"
fi

if [[ "$SMV_EXE_SET" == "0" ]]; then
  SMV_EXE="$FIREMODELS_ROOT/smv/Build/smokeview/$SMV_BUILD_TARGET/smokeview_osx"
fi

if [[ "$MANUALS_DOWNLOAD_DIR_SET" == "0" ]]; then
  MANUALS_DOWNLOAD_DIR="$STAGE_ROOT/release-manuals"
fi

update_bundle_repos

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

if git -C "$REPO_ROOT" describe --tags --long --dirty --always >/dev/null 2>&1; then
  EXTRACT_NAME="$(git -C "$REPO_ROOT" describe --tags --long --dirty --always)"
else
  EXTRACT_NAME="$DIST_NAME"
fi
EXTRACT_NAME="$(sanitize_name "$EXTRACT_NAME")"

resolve_manual_sources
build_cfast_executable
build_cedit_app
build_smokeview_executable

require_file "$CFAST_EXE" "CFAST executable"
require_file "$EXAMPLE_FILE" "CFAST example file"

mkdir -p "$OUTPUT_DIR"

DIST_DIR="$STAGE_ROOT/$DIST_NAME/CFAST8"
DMG_README="$STAGE_ROOT/$DIST_NAME/README.txt"
INSTALL_COMMAND="$STAGE_ROOT/$DIST_NAME/Terminal Install Menu.command"
DMG_NAME="$(sanitize_name "$DIST_NAME").dmg"
DMG_PATH="$OUTPUT_DIR/$DMG_NAME"

echo "*** Staging CFAST macOS bundle"
echo "*** Distribution: $DIST_NAME"
echo "*** Stage: $DIST_DIR"
echo "*** Output: $DMG_PATH"

rm -rf "$STAGE_ROOT/$DIST_NAME"
mkdir -p "$DIST_DIR/bin" "$DIST_DIR/Documentation" "$DIST_DIR/Examples"

if [[ "$INCLUDE_CEDIT" == "1" ]]; then
  if [[ -d "$CEDIT_APP" ]]; then
    echo "*** Adding CEditQt"
    copy_dir "$CEDIT_APP" "$DIST_DIR/$APP_NAME.app"
  else
    echo "*** Warning: CEditQt app not found; continuing without CEditQt."
    echo "             cedit: $CEDIT_APP"
  fi
fi

copy_file "$CFAST_EXE" "$DIST_DIR/bin/cfast8_macos"
chmod +x "$DIST_DIR/bin/cfast8_macos"
ln -s cfast8_macos "$DIST_DIR/bin/cfast"
bundle_macos_runtime_libraries "$CFAST_EXE" "$DIST_DIR/bin/cfast8_macos" "$DIST_DIR/lib"

copy_file "$EXAMPLE_FILE" "$DIST_DIR/Examples/Users_Guide_Example.in"

copy_manuals

write_cfast_vars "$DIST_DIR/bin/CFASTVARS.sh"
write_readme "$DIST_DIR/README.txt"
write_dmg_readme "$DMG_README"
write_install_command "$INSTALL_COMMAND" "$EXTRACT_NAME"

if [[ "$INCLUDE_SMOKEVIEW" == "1" ]]; then
  if [[ -f "$SMV_EXE" && -d "$SMV_BUNDLE_DIR" ]]; then
    echo "*** Adding Smokeview"
    mkdir -p "$DIST_DIR/SMV6"
    copy_file "$SMV_EXE" "$DIST_DIR/SMV6/smokeview"
    chmod +x "$DIST_DIR/SMV6/smokeview"
    bundle_macos_runtime_libraries "$SMV_EXE" "$DIST_DIR/SMV6/smokeview" "$DIST_DIR/lib"
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
  if ! "$PYTHON_EXE" -c "import dmgbuild" >/dev/null 2>&1; then
    echo "***error: dmgbuild is not available in $PYTHON_EXE."
    exit 1
  fi

  if ! create_compressed_dmg; then
    echo "***error: macOS DMG creation failed."
    echo "         Confirm dmgbuild is installed and hdiutil can create disk images."
    exit 1
  fi

  echo "*** DMG created:"
  echo "    $DMG_PATH"
else
  echo "*** Bundle staged:"
  echo "    $STAGE_ROOT/$DIST_NAME"
fi

upload_macos_bundle

#!/usr/bin/env bash
set -euo pipefail

ORIGINAL_ARGS=("$@")
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
FIREMODELS_ROOT="$(cd "$REPO_ROOT/.." && pwd)"

APP_NAME="cedit"
DIST_NAME=""
OUTPUT_DIR="$REPO_ROOT/Build/bundle/linux"
STAGE_ROOT="$REPO_ROOT/Build/bundle/stage"
CFAST_EXE="$REPO_ROOT/Build/CFAST/gnu_linux/cfast8_linux"
CEDIT_APP="$REPO_ROOT/Build/CeditQt/linux/$APP_NAME"
EXAMPLE_FILE="$REPO_ROOT/Utilities/for_bundle/Bin/Data/Users_Guide_Example.in"
SMV_EXE="$FIREMODELS_ROOT/smv/Build/smokeview/gnu_linux/smokeview_linux"
SMV_EXE_SET=0
SMV_BUNDLE_DIR="$FIREMODELS_ROOT/smv/Build/for_bundle"
SMV_BUILD_TARGET="gnu_linux"
INCLUDE_CEDIT=1
INCLUDE_SMOKEVIEW=1
BUILD_SMOKEVIEW=1
CREATE_TARBALL=1
UPDATE_REPOS=1
UPDATE_BRANCH="master"

usage()
{
  echo "Usage: build_linux_bundle.sh [options]"
  echo ""
  echo "Stages a CFAST Linux bundle and creates a .tar.gz archive."
  echo ""
  echo "Options:"
  echo "  --name name              Distribution folder name"
  echo "  --output-dir path        Output directory for the tarball"
  echo "  --stage-dir path         Temporary staging directory"
  echo "  --cfast-exe path         CFAST executable to bundle"
  echo "  --cedit-app path         CEditQt PyInstaller app directory to bundle"
  echo "  --example path           Example .in file to bundle"
  echo "  --smokeview-exe path     Smokeview executable to bundle"
  echo "  --smokeview-data path    Smokeview for_bundle directory"
  echo "  --smokeview-build-target target Smokeview build target: gnu_linux, intel_linux, or clang_linux"
  echo "  --update-branch branch   Branch to update before building"
  echo "  --no-update-repos        Do not update the CFAST, Smokeview, and FDS repos"
  echo "  --no-build-smokeview     Do not build Smokeview before bundling"
  echo "  --no-cedit               Do not bundle CEditQt"
  echo "  --no-smokeview           Do not bundle Smokeview files"
  echo "  --no-tarball             Stage files only"
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

run_checked()
{
  local description="$1"
  shift

  if ! "$@"; then
    echo "***error: $description failed."
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

  mkdir -p "$to_path"
  cp -a "$from_path"/. "$to_path"/
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
  if [[ "${CFAST_LINUX_BUNDLE_REEXECUTED:-}" == "1" ]]; then
    return 0
  fi

  update_git_repo cfast "$REPO_ROOT" && updated_repo=1
  update_git_repo smv "$FIREMODELS_ROOT/smv" && updated_repo=1
  update_git_repo fds "$FIREMODELS_ROOT/fds" && updated_repo=1

  if [[ "$updated_repo" != "1" ]]; then
    return 0
  fi

  echo "*** Re-starting Linux bundle script after repo updates"
  export CFAST_LINUX_BUNDLE_REEXECUTED=1
  if [[ "${#ORIGINAL_ARGS[@]}" -gt 0 ]]; then
    exec "$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")" "${ORIGINAL_ARGS[@]}"
  else
    exec "$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
  fi
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
  require_file "$smokeview_dir/make_smokeview.sh" "Smokeview Linux build script"

  echo "*** Building Smokeview Linux libraries ($SMV_BUILD_TARGET)"
  (cd "$libs_dir" && run_checked "Smokeview $SMV_BUILD_TARGET libraries build" bash ./make_LIBS.sh)

  echo "*** Building Smokeview Linux executable ($SMV_BUILD_TARGET)"
  (cd "$smokeview_dir" && run_checked "Smokeview $SMV_BUILD_TARGET build" bash ./make_smokeview.sh)
}

linux_runtime_library_name()
{
  local library_name="$1"

  case "$library_name" in
    libgcc_s.so*|libgfortran.so*|libgomp.so*|libquadmath.so*|libstdc++.so*)
      return 0
      ;;
    libfabric.so*|libiomp5.so*|libimf.so*|libintlc.so*|libirng.so*)
      return 0
      ;;
    libifcore.so*|libifcoremt.so*|libifport.so*|libirc.so*)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

copy_linux_runtime_libraries()
{
  local binary_path="$1"
  local lib_dir="$2"
  local dependency_path
  local library_name
  local destination_path

  mkdir -p "$lib_dir"

  while read -r dependency_path; do
    if [[ "$dependency_path" == "" || ! -f "$dependency_path" ]]; then
      continue
    fi

    library_name="$(basename "$dependency_path")"
    if ! linux_runtime_library_name "$library_name"; then
      continue
    fi

    destination_path="$lib_dir/$library_name"
    if [[ ! -f "$destination_path" ]]; then
      echo "*** Copying Linux runtime library: $library_name"
      cp -L -p "$dependency_path" "$destination_path"
      chmod u+w "$destination_path"
    fi
  done < <(
    ldd "$binary_path" | awk '
      /=>/ { print $3; next }
      /^[[:space:]]*\// { print $1; next }
    '
  )
}

bundle_linux_runtime_libraries()
{
  local binary_path="$1"
  local lib_dir="$2"

  copy_linux_runtime_libraries "$binary_path" "$lib_dir"
}

write_cfast_vars_sh()
{
  local out_file="$1"

  cat > "$out_file" <<'EOF'
# Source this file from bash or zsh to add CFAST, CEditQt, and Smokeview to
# PATH.

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

    if [ -d "$CFAST_HOME/lib" ]; then
        case ":${LD_LIBRARY_PATH:-}:" in
            *":$CFAST_HOME/lib:"*) ;;
            *) export LD_LIBRARY_PATH="$CFAST_HOME/lib:${LD_LIBRARY_PATH:-}" ;;
        esac
    fi
fi

unset _cfast_vars_file _cfast_vars_dir
EOF

  chmod +x "$out_file"
}

write_cedit_launcher()
{
  local out_file="$1"

  cat > "$out_file" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cfast_home="$(cd "$script_dir/.." && pwd)"
cedit_exe="$cfast_home/CEditQt/cedit/cedit"

export CFAST_HOME="$cfast_home"

case ":${PATH:-}:" in
    *":$CFAST_HOME/bin:"*) ;;
    *) export PATH="$CFAST_HOME/bin:${PATH:-}" ;;
esac

if [[ -d "$CFAST_HOME/SMV6" ]]; then
    case ":${PATH:-}:" in
        *":$CFAST_HOME/SMV6:"*) ;;
        *) export PATH="$CFAST_HOME/SMV6:${PATH:-}" ;;
    esac
fi

if [[ -d "$CFAST_HOME/lib" ]]; then
    case ":${LD_LIBRARY_PATH:-}:" in
        *":$CFAST_HOME/lib:"*) ;;
        *) export LD_LIBRARY_PATH="$CFAST_HOME/lib:${LD_LIBRARY_PATH:-}" ;;
    esac
fi

if [[ ! -x "$cedit_exe" ]]; then
    echo "***error: CEditQt executable not found: $cedit_exe"
    exit 1
fi

exec "$cedit_exe" "$@"
EOF

  chmod +x "$out_file"
}

write_readme()
{
  local out_file="$1"

  cat > "$out_file" <<'EOF'
CFAST Linux Bundle
==================

This bundle contains:

- bin/cfast and bin/cfast8_linux
- bin/CFASTVARS.sh
- bin/cedit, if CEditQt was available when the bundle was made
- CEditQt/cedit, if CEditQt was available when the bundle was made
- Documentation/*.pdf
- Examples/Users_Guide_Example.in
- lib/compiler runtime libraries copied from the build host, if needed
- SMV6/smokeview, if Smokeview was available when the bundle was made

To use CFAST from bash or zsh:

    source /path/to/CFAST/CFAST8/bin/CFASTVARS.sh
    cfast /path/to/CFAST/CFAST8/Examples/Users_Guide_Example.in

To launch CEditQt from a terminal:

    cedit

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
    --update-branch)
      UPDATE_BRANCH="$2"
      shift 2
      ;;
    --no-update-repos)
      UPDATE_REPOS=0
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
    --no-tarball)
      CREATE_TARBALL=0
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

if [[ "$(uname)" != "Linux" ]]; then
  echo "***error: Linux bundles must be built on Linux."
  exit 1
fi

case "$SMV_BUILD_TARGET" in
  gnu_linux|intel_linux|clang_linux)
    ;;
  *)
    echo "***error: unsupported Smokeview Linux build target: $SMV_BUILD_TARGET"
    exit 1
    ;;
esac

if [[ "$SMV_EXE_SET" == "0" ]]; then
  SMV_EXE="$FIREMODELS_ROOT/smv/Build/smokeview/$SMV_BUILD_TARGET/smokeview_linux"
fi

update_bundle_repos

if [[ "$DIST_NAME" == "" ]]; then
  if git -C "$REPO_ROOT" describe --tags --dirty --always >/dev/null 2>&1; then
    cfast_version="$(git -C "$REPO_ROOT" describe --tags --dirty --always)"
    if [[ "$cfast_version" == CFAST* ]]; then
      DIST_NAME="$cfast_version-linux"
    else
      DIST_NAME="CFAST-$cfast_version-linux"
    fi
  else
    DIST_NAME="CFAST-linux"
  fi
fi

require_file "$CFAST_EXE" "CFAST executable"
require_file "$EXAMPLE_FILE" "CFAST example file"
build_smokeview_executable

mkdir -p "$OUTPUT_DIR"

DIST_DIR="$STAGE_ROOT/$DIST_NAME/CFAST/CFAST8"
TARBALL_NAME="$(sanitize_name "$DIST_NAME").tar.gz"
TARBALL_PATH="$OUTPUT_DIR/$TARBALL_NAME"

echo "*** Staging CFAST Linux bundle"
echo "*** Distribution: $DIST_NAME"
echo "*** Stage: $DIST_DIR"
echo "*** Output: $TARBALL_PATH"

rm -rf "$STAGE_ROOT/$DIST_NAME"
mkdir -p "$DIST_DIR/bin" "$DIST_DIR/Documentation" "$DIST_DIR/Examples"

copy_file "$CFAST_EXE" "$DIST_DIR/bin/cfast8_linux"
chmod +x "$DIST_DIR/bin/cfast8_linux"
ln -s cfast8_linux "$DIST_DIR/bin/cfast"
bundle_linux_runtime_libraries "$DIST_DIR/bin/cfast8_linux" "$DIST_DIR/lib"

copy_file "$EXAMPLE_FILE" "$DIST_DIR/Examples/Users_Guide_Example.in"

copy_file "$REPO_ROOT/Manuals/CFAST_Configuration_Guide/CFAST_Configuration_Guide.pdf" "$DIST_DIR/Documentation/CFAST_Configuration_Guide.pdf"
copy_file "$REPO_ROOT/Manuals/CFAST_Tech_Ref/CFAST_Tech_Ref.pdf" "$DIST_DIR/Documentation/CFAST_Tech_Ref.pdf"
copy_file "$REPO_ROOT/Manuals/CFAST_Users_Guide/CFAST_Users_Guide.pdf" "$DIST_DIR/Documentation/CFAST_Users_Guide.pdf"
copy_file "$REPO_ROOT/Manuals/CFAST_Validation_Guide/CFAST_Validation_Guide.pdf" "$DIST_DIR/Documentation/CFAST_Validation_Guide.pdf"

write_cfast_vars_sh "$DIST_DIR/bin/CFASTVARS.sh"
write_readme "$DIST_DIR/README.txt"

if [[ "$INCLUDE_CEDIT" == "1" ]]; then
  if [[ -d "$CEDIT_APP" ]]; then
    echo "*** Adding CEditQt"
    copy_dir "$CEDIT_APP" "$DIST_DIR/CEditQt/$APP_NAME"
    write_cedit_launcher "$DIST_DIR/bin/cedit"
  else
    echo "*** Warning: CEditQt app not found; continuing without CEditQt."
    echo "             cedit: $CEDIT_APP"
  fi
fi

if [[ "$INCLUDE_SMOKEVIEW" == "1" ]]; then
  if [[ -f "$SMV_EXE" && -d "$SMV_BUNDLE_DIR" ]]; then
    echo "*** Adding Smokeview"
    mkdir -p "$DIST_DIR/SMV6"
    copy_file "$SMV_EXE" "$DIST_DIR/SMV6/smokeview"
    chmod +x "$DIST_DIR/SMV6/smokeview"
    bundle_linux_runtime_libraries "$DIST_DIR/SMV6/smokeview" "$DIST_DIR/lib"
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

if [[ "$CREATE_TARBALL" == "1" ]]; then
  rm -f "$TARBALL_PATH"
  echo "*** Creating tarball"
  tar -C "$STAGE_ROOT/$DIST_NAME" -czf "$TARBALL_PATH" CFAST
  echo "*** Tarball created:"
  echo "    $TARBALL_PATH"
else
  echo "*** Bundle staged:"
  echo "    $STAGE_ROOT/$DIST_NAME"
fi

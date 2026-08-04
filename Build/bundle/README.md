# CFAST Bundle Builds

This directory contains the platform bundle scripts for CFAST 8. Run the
commands from the CFAST repository root unless noted otherwise.

## Quick Start

### macOS

Create the DMG, building CFAST and CEditQt first:

```bash
Build/bundle/build_macos_bundle.sh
```

The DMG is written to `Build/bundle/macos`.

To use manuals from the `CFAST_TEST` release and upload the finished DMG back
to the test bundle release:

```bash
Build/bundle/build_macos_bundle.sh --manuals-from-release --upload
```

For a macOS scheduled job, use the wrapper script:

```bash
Build/bundle/run_macos_bundle.sh
```

### Linux

Build CFAST and CEditQt, then create the tarball. The bundle script updates
the repos and rebuilds Smokeview before staging:

```bash
Build/CeditQt/build_linux_app.sh
Build/bundle/build_linux_bundle.sh
```

The tarball is written to `Build/bundle/linux`.

### Windows

From PowerShell or Command Prompt, create the self-extracting installer:

```powershell
python Build\bundle\build_windows_bundle.py --manuals-from-release
```

The Windows script builds `Build\CFAST\intel_win\cfast8_win.exe` and CEditQt by
default before staging the bundle. The installer is written to
`Build\bundle\windows`.

To upload the installer to the test bundle release after it is built:

```powershell
python Build\bundle\build_windows_bundle.py --manuals-from-release --upload
```

For a Windows scheduled task, use the wrapper script:

```powershell
Build\bundle\run_windows_bundle.ps1
```

By default, `--upload` pushes to `firemodels/test_bundles` release tag
`CFAST_TEST`. Override this with:

```powershell
python Build\bundle\build_windows_bundle.py --manuals-from-release --upload --upload-release-repo firemodels/test_bundles --upload-release-tag CFAST_TEST
```

The upload step requires `gh` to be installed and authenticated.

## Inputs

The macOS, Linux, and Windows bundle scripts update `cfast`, `smv`, and `fds`
from `master` before building unless `--no-update-repos` is supplied. The `fds`
repo is updated only for the shared Python environment; FDS is not built by
these scripts. The `exp` repo is not updated during bundle assembly.

The bundle scripts stage:

- `cfast8_*` and a generic `cfast` launcher/copy
- CFAST manuals
- `Utilities/for_bundle/Bin/Data/Users_Guide_Example.in`
- Extra CFAST example inputs from `Utilities/for_bundle/Bin/Data`, such as `Large_Building.in`
- CEditQt, when available
- Smokeview, rebuilt from the local `smv` checkout unless disabled
- runtime libraries needed by the bundled executables

The macOS and Windows scripts build CFAST, CEditQt, and Smokeview unless
disabled. The Linux script currently builds Smokeview, but assumes the CFAST
executable and CEditQt app have already been built.

## Dependencies

Common:

- Python 3
- CFAST build tools for the selected compiler/platform
- CEditQt Python dependencies
- PyInstaller for CEditQt app builds

macOS:

- `dmgbuild`
- GitHub CLI (`gh`) for `--manuals-from-release` and `--upload`

Windows:

- Intel Fortran build environment available to `Build\CFAST\intel_win\make_cfast.bat`
- PyInstaller
- Pillow
- GitHub CLI (`gh`) for `--manuals-from-release` and `--upload`

Linux:

- GNU or Intel CFAST build environment
- PyInstaller for the CEditQt app build

## Useful Options

Show all options:

```bash
Build/bundle/build_macos_bundle.sh --help
Build/bundle/build_linux_bundle.sh --help
python Build/bundle/build_windows_bundle.py --help
```

Common options:

- `--name`: set the distribution name
- `--output-dir`: set the output directory
- `--stage-dir`: set the staging directory
- `--update-branch`: branch to update before building
- `--no-update-repos`: do not update `cfast`, `smv`, and `fds`
- `--cfast-exe`: use a specific CFAST executable
- `--smokeview-exe`: use a specific Smokeview executable
- `--no-build-smokeview`: do not build Smokeview before bundling
- `--no-smokeview`: omit Smokeview
- `--manuals-from-release`: download manuals from a GitHub release
- `--strict-revision`: require downloaded manuals to match the local CFAST revision
- `--upload`: upload the created bundle to a GitHub release

macOS and Windows options:

- `--no-build-cfast`: do not build CFAST before bundling
- `--no-build-cedit`: do not build CEditQt before bundling

Windows-only options:

- `--no-uac-admin`: build the installer without requesting administrator privileges

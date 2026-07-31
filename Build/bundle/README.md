# CFAST Bundle Builds

This directory contains the platform bundle scripts for CFAST 8. Run the
commands from the CFAST repository root unless noted otherwise.

## Quick Start

### macOS

Build CEditQt, then create the DMG:

```bash
Build/CeditQt/build_macos_app.sh
Build/bundle/build_macos_bundle.sh
```

The DMG is written to `Build/bundle/macos`.

### Linux

Build CEditQt, then create the tarball:

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

By default, `--upload` pushes to `firemodels/test_bundles` release tag
`CFAST_TEST`. Override this with:

```powershell
python Build\bundle\build_windows_bundle.py --manuals-from-release --upload --upload-release-repo firemodels/test_bundles --upload-release-tag CFAST_TEST
```

The upload step requires `gh` to be installed and authenticated.

## Inputs

The bundle scripts stage:

- `cfast8_*` and a generic `cfast` launcher/copy
- CFAST manuals
- `Utilities/for_bundle/Bin/Data/Users_Guide_Example.in`
- CEditQt, when available
- Smokeview, when available
- runtime libraries needed by the bundled executables

The macOS and Linux scripts assume the CFAST executable and CEditQt app have
already been built. The Windows script builds CFAST and CEditQt unless
`--no-build-cfast` or `--no-build-cedit` is supplied.

## Dependencies

Common:

- Python 3
- CFAST build tools for the selected compiler/platform
- CEditQt Python dependencies
- PyInstaller for CEditQt app builds

macOS:

- `dmgbuild`

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
- `--cfast-exe`: use a specific CFAST executable
- `--smokeview-exe`: use a specific Smokeview executable
- `--no-smokeview`: omit Smokeview

Windows-only options:

- `--manuals-from-release`: download manuals from a GitHub release
- `--strict-revision`: require downloaded manuals to match the local CFAST revision
- `--upload`: upload the created installer to a GitHub release
- `--no-uac-admin`: build the installer without requesting administrator privileges


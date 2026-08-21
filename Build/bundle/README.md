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

### Signed and notarized macOS releases

Gatekeeper accepts a downloaded application only after it has a Developer ID
signature and has been notarized by Apple. The bundle build takes its Developer
ID identity from `CODESIGN_ID`, keeping it out of the repository. It signs
CEditQt and its nested code, CFAST, Smokeview, and bundled runtime libraries
with the hardened runtime, then signs the finished DMG. When notarization is
enabled, the build submits the DMG to Apple and staples the resulting ticket.

For a macOS scheduled job, use the wrapper script:

```bash
Build/bundle/run_macos_bundle.sh
```

### Linux

Build CEditQt, then create the tarball. The bundle script updates the repos,
rebuilds CFAST and Smokeview, and builds the CFAST manuals before staging. The
Linux tarball includes the built manuals. The same PDFs and revision information
are uploaded for the macOS and Windows bundle builds:

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

The macOS, Linux, and Windows bundle scripts reset and clean the local `cfast`
checkout, fetch `git@github.com:firemodels/cfast.git`, and use the latest
central `master` by default. Use `--cfast-tag` to build a specific CFAST tag.
They also fresh-clone `smv` from `git@github.com:firemodels/smv.git` into the
parallel firemodels workspace before building unless `--no-update-repos` is
supplied.

The macOS and Windows scripts fresh-clone `fds` from
`git@github.com:firemodels/fds.git` into the parallel firemodels workspace and
build `.github/fds_python_env` from that checkout unless `--python` is supplied.
Linux also fresh-clones the parallel `fds` checkout during repo updates. FDS is
used only for the shared Python environment; FDS is not built by these scripts.
The `exp` repo is not updated during bundle assembly.

The bundle scripts stage:

- `cfast8_*` and a generic `cfast` launcher/copy
- CFAST manuals
- `Utilities/for_bundle/Bin/Data/Users_Guide_Example.in`
- Extra CFAST example inputs from `Utilities/for_bundle/Bin/Data`, such as `Large_Building.in`
- CEditQt, when available
- Smokeview, rebuilt from the local `smv` checkout unless disabled
- runtime libraries needed by the bundled executables

The macOS and Windows scripts build CFAST, CEditQt, and Smokeview unless
disabled. The Linux script builds CFAST and Smokeview, but assumes the CEditQt
app has already been built.

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
- LaTeX with `biber` for the CFAST manuals
- GitHub CLI (`gh`) to upload the built manuals

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
- `--no-update-repos`: do not sync `cfast` or fresh-clone `smv`/`fds`
- `--cfast-tag`: checkout a specific CFAST tag after fetching central
- `--cfast-build-target`: CFAST build target
- `--cfast-exe`: use a specific CFAST executable
- `--no-build-cfast`: do not build CFAST before bundling
- `--smokeview-exe`: use a specific Smokeview executable
- `--no-build-smokeview`: do not build Smokeview before bundling
- `--no-smokeview`: omit Smokeview

Linux-only options:

- `--no-build-manuals`: do not build the CFAST PDF manuals
- `--no-upload-manuals`: skip publishing built manuals for a local-only run

macOS and Windows options:

- `--python`: use a specific Python executable and skip fresh FDS Python env setup
- `--no-build-cfast`: do not build CFAST before bundling
- `--no-build-cedit`: do not build CEditQt before bundling

Windows-only options:

- `--no-uac-admin`: build the installer without requesting administrator privileges

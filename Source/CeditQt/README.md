# CEdit Qt

Python/PySide6 prototype for a CFAST input-file editor.

## Running

No compilation step is required. Run the GUI from this directory:

    cd Source/CeditQt
    python cedit_qt.py

The Python environment must provide PySide6 and matplotlib.

## Building a macOS App

To build a standalone macOS app, first install PyInstaller in the Python
environment that already has PySide6 and matplotlib:

    python -m pip install pyinstaller

Then run:

    cd Source/CeditQt
    ./build_macos_app.sh --python python

The app is written to:

    Build/CeditQt/macos/CFAST Editor (CEdit).app

By default, the build script uses `assets/CeditQt.icns` when present,
otherwise it passes `assets/CeditQt.png` to PyInstaller.

## Building a macOS Bundle

After the CFAST executable, CEditQt app, manuals, and Smokeview files have
been built, install the DMG builder in the active Python environment:

    python -m pip install dmgbuild

Then stage the macOS bundle and create a DMG from the repository root:

    Build/scripts/build_macos_bundle.sh

The DMG is written under:

    Build/bundle/macos

## Running CFAST from CEdit Qt

In the macOS bundle, CEdit Qt first looks for the bundled executable:

    CFAST/bin/cfast

If a bundled executable is not found, CEdit Qt runs:

    cfast

so the CFAST executable must be available on your PATH when running outside the
bundle.

To use a specific executable, for example a local development build, choose:

    File > Set CFAST Executable...

To return to the bundled/PATH default, choose:

    File > Use Bundled CFAST or CFAST from PATH

The Geometry and View controls also use Smokeview. In the macOS bundle, CEdit Qt
first looks for:

    CFAST/SMV6/smokeview

If a bundled Smokeview executable is not found, CEdit Qt runs:

    smokeview

so the Smokeview executable must be available on your PATH when running outside
the bundle. To use a specific executable, choose:

    File > Set Smokeview Executable...

To return to the bundled/PATH default, choose:

    File > Use Bundled Smokeview or Smokeview from PATH

## Optional syntax check

To check the Python files without launching the GUI:

    cd Source/CeditQt
    python -m py_compile cedit_qt.py main_window.py cfast_case.py cfast_writer.py tabs/*.py widgets/*.py

## Verification UI test harness

To load verification cases through the Qt UI, write the CFAST input files,
run CFAST, and leave generated CSV outputs ready for the official verification
scripts:

    cd Source/CeditQt
    python run_verification_ui_tests.py --cfast-exe ../../Build/CFAST/gnu_osx/cfast7_osx

To test only one case while developing:

    cd Source/CeditQt
    python run_verification_ui_tests.py --cfast-exe ../../Build/CFAST/gnu_osx/cfast7_osx --case basic_tempequilib --keep-work-dir

The harness copies `Verification/` to a temporary work tree before running, so
the checked-in verification input files are not modified.

The optional `--compare-targets` flag runs an experimental built-in CSV
comparison. Use `Utilities/Python/CFAST_verification_script.py` as the
authoritative verification statistics check.

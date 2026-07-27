# CEdit Qt

Python/PySide6 interface for editing and running CFAST input files.

## Running

The Python environment must provide PySide6 and matplotlib.

Run the GUI from this directory:

    cd Source/CeditQt
    python cedit_qt.py

## Dependencies

Install the dependencies needed for the action you are running:

    python -m pip install pyinstaller
    python -m pip install pillow
    python -m pip install dmgbuild

PyInstaller is required for standalone app builds. Pillow is required for
Windows icon generation. dmgbuild is required for the macOS DMG.

## macOS App

Run from the repository root:

    Build/CeditQt/build_macos_app.sh --python python

Output:

    Build/CeditQt/macos/CFAST Editor (CEdit).app

## macOS Bundle

Run from the repository root:

    Build/bundle/build_macos_bundle.sh

Output:

    Build/bundle/macos

## Linux App

Run from the repository root on Linux:

    Build/CeditQt/build_linux_app.sh

Output:

    Build/CeditQt/linux/CFAST Editor (CEdit)

## Linux Bundle

Run from the repository root on Linux:

    Build/bundle/build_linux_bundle.sh

Output:

    Build/bundle/linux

After extracting the tarball, source the startup file before using CFAST from a
terminal:

    source /path/to/CFAST/bin/CFASTVARS.sh

## Windows App

Run from the repository root on Windows:

    python Build\CeditQt\build_windows_app.py

Output:

    Build\CeditQt\windows\CFAST Editor (CEdit)

## Windows Bundle

Run from the repository root on Windows:

    python Build\bundle\build_windows_bundle.py

Output:

    Build\bundle\windows

The installer writes to:

    C:\Program Files\firemodels\CFAST

For a test installer that should not request administrator privileges:

    python Build\bundle\build_windows_bundle.py --no-uac-admin

Shortcut options:

    CFAST-...-windows.exe --desktop-shortcut
    CFAST-...-windows.exe --no-desktop-shortcut

## CFAST and Smokeview Paths

In a bundle, CEdit Qt first looks for the bundled CFAST and Smokeview
executables. Outside a bundle, CEdit Qt uses `cfast` and `smokeview` from
`PATH`.

To select a specific executable:

    File > Set CFAST Executable...
    File > Set Smokeview Executable...

To return to the bundled/PATH default:

    File > Use Bundled CFAST or CFAST from PATH
    File > Use Bundled Smokeview or Smokeview from PATH

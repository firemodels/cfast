# CEdit Qt

Python/PySide6 prototype for a CFAST input-file editor.

## Running

No compilation step is required. Run the GUI from this directory:

    cd Source/CeditQt
    python cedit_qt.py

The Python environment must provide PySide6 and matplotlib.

## Running CFAST from CEdit Qt

By default, CEdit Qt runs:

    cfast

so the CFAST executable must be available on your PATH.

To use a specific executable, for example a local development build, choose:

    File > Set CFAST Executable...

To return to the PATH-based default, choose:

    File > Use CFAST from PATH

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

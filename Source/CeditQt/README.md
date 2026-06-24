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

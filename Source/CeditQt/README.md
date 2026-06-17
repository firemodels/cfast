# CEdit Qt Prototype

Experimental Python/PySide6 prototype for a new CEdit front end for CFAST.

This prototype assumes it is run from the existing FDS/CFAST Python development environment.

Run with:

    python cedit_qt.py

Current status:

- Main CEdit-style window
- Model object dock
- Placeholder tabs for Model, Compartments, Vents, Targets, and Thermal Properties
- Working Fires tab
- Editable HRR ramp table
- Live HRR ramp preview plot

Near-term goal:

Build a vertical slice that exports a valid CFAST input file, `casename.in`, from the GUI.


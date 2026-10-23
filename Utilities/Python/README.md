# Processing the Verification and Validation Cases

This folder contains Python scripts that read the output of the CFAST verification and validation cases and create figures for the CFAST Manuals. Before running these scripts, run the [verification cases](https://github.com/firemodels/cfast/tree/master/Verification/scripts) and the [validation cases](https://github.com/firemodels/cfast/tree/master/Validation/scripts).

CFAST uses the same Python package requirements as FDS. Keep an FDS checkout beside CFAST for the shared `fdsplotlib` module. From this directory, create and activate CFAST's environment on Linux/macOS with `source ./setup_python_env.sh`; in PowerShell use `. .\setup_python_env.ps1`, or in Command Prompt use `call setup_python_env.bat`. The environment is stored in `cfast/.github/cfast_python_env` and installed from `cfast/.github/requirements.txt`. Setup reuses existing environments and installs changed requirements in place; `--batchmode` is accepted for unattended use. Python 3.7 is the setup minimum inherited from FDS, but current packages may require a newer Python version.

An already provisioned FDS environment can also be activated and used directly. CFAST CI prefers that sibling FDS environment and only sets up CFAST's environment when the FDS interpreter is absent; `CFAST_CI_PYTHON` explicitly overrides that selection.

After the V&V cases have completed and the environment is active, run:
```
python CFAST_verification_script.py
python CFAST_validation_script.py
```
After these scripts have completed, you can compile the [CFAST Manuals](https://github.com/firemodels/cfast/tree/master/Manuals).

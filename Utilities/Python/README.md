# Processing the Verification and Validation Cases

This folder contains Python scripts that read the output of the CFAST verification and validation cases and create figures for the CFAST Manuals. Before running these scripts, run the [verification cases](https://github.com/firemodels/cfast/tree/master/Verification/scripts) and the [validation cases](https://github.com/firemodels/cfast/tree/master/Validation/scripts).

Use the shared FDS Python environment for CFAST and the other Firemodels repositories. Keep it active when moving between repositories; no environment switch or separate `fdsplotlib` installation is needed. Keep an FDS checkout beside CFAST for the shared utilities. CFAST's `.github/requirements.txt` mirrors the shared package list but omits the cross-repository editable path so Dependabot can process it.

CFAST CI prefers the sibling `fds/.github/fds_python_env` environment; `CFAST_CI_PYTHON` explicitly overrides that selection. The existing `setup_python_env.sh`, `.ps1`, and `.bat` scripts provide a CFAST-specific fallback only when the FDS environment is unavailable.

After the V&V cases have completed and the environment is active, run:
```
python CFAST_verification_script.py
python CFAST_validation_script.py
```
After these scripts have completed, you can compile the [CFAST Manuals](https://github.com/firemodels/cfast/tree/master/Manuals).

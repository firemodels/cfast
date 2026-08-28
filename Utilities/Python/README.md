# Processing the Verification and Validation Cases

This folder contains Python scripts that read the output of the CFAST verification and validation cases and create figures for the CFAST Manuals. Before running these scripts, run the [verification cases](https://github.com/firemodels/cfast/tree/master/Verification/scripts) and the [validation cases](https://github.com/firemodels/cfast/tree/master/Validation/scripts).

After the V&V cases have completed, follow the [instructions](https://github.com/firemodels/fds/wiki/Python-Setting-Up-Your-Environment) for installing and activating a special Python environment used by CFAST and FDS for post-processing. This special evnironment ensures that the proper modules are installed on your system. Once that is done, then run the scripts:
```
python CFAST_verification_script.py
python CFAST_validation_script.py
```
After these scripts have completed, you can compile the [CFAST Manuals](https://github.com/firemodels/cfast/tree/master/Manuals).

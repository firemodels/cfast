# CFAST Verification

These folders contain the CFAST verification input files that are run by the CFASTbot script contained in the repository [firemodels/bot](https://github.com/firemodels/bot). Detailed instructions for running and processing the verification cases are found in the wiki [CFASTBot](https://github.com/firemodels/cfast/wiki/CFASTbot).

The verification cases can also be run locally without CFASTbot:

    python run_cfast_verification_cases.py --cfast-exe ../Build/CFAST/gnu_osx/cfast7_osx

After the cases complete, run the plotting and statistics script from `Utilities/Python`:

    cd ../Utilities/Python
    python CFAST_verification_script.py

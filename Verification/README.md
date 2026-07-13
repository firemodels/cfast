# CFAST Verification

These folders contain the CFAST verification input files that are run by the CFASTbot script contained in the repository [firemodels/bot](https://github.com/firemodels/bot). Detailed instructions for running and processing the verification cases are found in the wiki [CFASTBot](https://github.com/firemodels/cfast/wiki/CFASTbot).

The verification cases can also be run locally or submitted to a cluster without CFASTbot:

    ./scripts/Run_CFAST_Cases.sh -I gnu -q batch

For a foreground run on a local machine:

    ./scripts/Run_CFAST_Cases.sh -I gnu -q none

After the cases complete, run the plotting and statistics script from `Utilities/Python`:

    cd ../Utilities/Python
    python CFAST_verification_script.py

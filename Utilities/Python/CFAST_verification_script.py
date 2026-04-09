#!$FIREMODELS/fds/.github/fds_python_env/bin/python
# McDermott
# 2 April 2024

import subprocess
import fdsplotlib
import matplotlib.pyplot as plt
import runpy
import importlib
importlib.reload(fdsplotlib) # use for development (while making changes to fdsplotlib.py)
print("Using:", fdsplotlib.__file__)

# If there is an error in one of the sub-scripts, print the message but do not stop the main script.

def safe_run(script_path):
    try:
        runpy.run_path(script_path, run_name="__main__")
        plt.clf()         # Clear the current figure (if any)
        plt.close('all')  # Close all open figure windows
    except Exception as exc:
        print(f"Error in {script_path}: {exc}")

# Scripts to run prior to dataplot

print("fire_ignition...");                     safe_run("./scripts/fire_ignition.py")
print("radiativefluxes...");                   safe_run("./scripts/radiativefluxes.py")
print("speciesmass...");                       safe_run("./scripts/speciesmass.py")
print("sprinkler_1...");                       safe_run("./scripts/sprinkler_1.py")
print("target_2...");                          safe_run("./scripts/target_2.py")

# Dataplot and scatplot options

Dataplot_Inputs_File = 'CFAST_verification_dataplot_inputs.csv';
Working_Dir = '../../Verification/';
Manuals_Dir = '../../Manuals/CFAST_Validation_Guide/';
Scatterplot_Inputs_File = 'CFAST_verification_scatterplot_inputs.csv';

# Statistics output options

Stats_Output = 'Verification'
Scatterplot_Dir = '../../Manuals/CFAST_Validation_Guide/SCRIPT_FIGURES/Scatterplots/'

# Run dataplot and scatplot scripts

saved_data, drange = fdsplotlib.dataplot(config_filename=Dataplot_Inputs_File,
                                         expdir=Working_Dir,
                                         cmpdir=Working_Dir,
                                         pltdir=Manuals_Dir,
                                         close_figs=True,
                                         verbose=True,
                                         plot_range=["all"]) # see notes below on plot_range

# ----- write saved_data, drange to disk -----
import pickle

# Save
with open("saved_data_verification.pkl", "wb") as f:
    pickle.dump((saved_data, drange), f)

# Later...
with open("saved_data_verification.pkl", "rb") as f:
    saved_data, drange = pickle.load(f)
#---------------------------------------------

fdsplotlib.scatplot(saved_data,drange,
                    Manuals_Dir=Manuals_Dir,
                    Scatterplot_Inputs_File=Scatterplot_Inputs_File,
                    Stats_Output=Stats_Output,
                    Scatterplot_Dir=Scatterplot_Dir,
                    verbose=True,
                    )

print("verification scripts completed successfully!")

# ------------------------------
# plot_range usage examples
#
# plot_range lets you select which rows of the config file to process.
# You can mix row numbers, ranges, and Dataname strings:
#
#  1. Single row by number (Spreadsheet-style, including header rows):
#       plot_range = [1995]
#
#  2. Inclusive ranges by "start:stop":
#       plot_range = ["5:9"]        # rows 5 through 9
#
#  3. Open-ended ranges:
#       plot_range = ["1995:"]      # from row 1995 to the end
#
#  4. Named selection by Dataname (case-insensitive):
#       plot_range = ["CSTB Tunnel", "Steckler Compartment"]
#
#  5. Mixed selection:
#       plot_range = [1, 2, "5:9", "CSTB Tunnel", "7000:"]
#
#  6. All rows:
#       plot_range = ["all"]
#
# Notes:
# - Row numbers are 1-based (like Spreadsheet).
# - Ranges are inclusive, e.g. "5:9" means 5,6,7,8,9.
# - "start:" runs to the last row.
# - Strings that are not ranges or "all" are matched to the Dataname column.
# ------------------------------

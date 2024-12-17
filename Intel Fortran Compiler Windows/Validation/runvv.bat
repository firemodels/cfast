@echo off

VandV_Calcs.exe CFAST_Pressure_Correction_Inputs.csv
del LLNL_Enclosure\LLNL_pressures.csv /Q
copy pressures.csv LLNL_Enclosure\LLNL_pressures.csv /Y

VandV_Calcs.exe CFAST_Temperature_Profile_inputs.csv
del Steckler_Compartment\profiles.csv /Q
copy profiles.csv Steckler_Compartment\profiles.csv /Y

VandV_Calcs.exe CFAST_Heat_Flux_Profile_inputs.csv
del Fleury_Heat_Flux\flux_profiles.csv /Q
copy flux_profiles.csv Fleury_Heat_Flux\flux_profiles.csv /Y

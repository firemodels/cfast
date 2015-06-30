..\VandV_Calcs\Release\VandV_Calcs.exe CFAST_Pressure_Correction_Inputs.csv
del LLNL_Enclosure\LLNL_pressures.csv /Q
copy pressures.csv LLNL_Enclosure\LLNL_pressures.csv /Y
..\VandV_Calcs\Release\VandV_Calcs.exe CFAST_Temperature_Profile_inputs.csv
del Steckler_Compartment\profiles.csv /Q
copy profiles.csv Steckler_Compartment\profiles.csv /Y

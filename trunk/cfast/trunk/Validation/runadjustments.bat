..\VandV_Calcs\Release\VandV_Calcs.exe CFAST_Pressure_Correction_Inputs.csv
copy pressures.csv LLNL_Enclosure\LLNL_pressures.csv /Y
..\VandV_Calcs\Release\VandV_Calcs.exe CFAST_Temperature_Profile_inputs.csv
copy profiles.csv Steckler_Compartment /Y

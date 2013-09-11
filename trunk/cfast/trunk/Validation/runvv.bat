@echo off

set vv=..\VandV_Calcs\intel_win_64\VandV_Calcs_win_64.exe

%vv% CFAST_Pressure_Correction_Inputs.csv
Rem copy pressures.csv ..\Valiidation\LLNL_Enclosure\LLNL_pressures.csv /Y

%vv% CFAST_Temperature_Profile_inputs.csv
Rem copy profiles.csv ..\Validation\Steckler_Compartment /Y

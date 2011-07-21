echo off
call cleanall.bat
echo NRC Users Guide Test Cases
cd "1 - Cabinet Fire in MCR"
echo 1 - Cabinet Fire in MCR
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Cabinet fire in MCR" /V 
echo 1 - Cabinet Fire in MCR No Ventilation
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Cabinet fire in MCR No Ventilation" /V 
cd ..\
cd "2 - Cabinet Fire in Switchgear"
echo 2 - Cabinet Fire in Switchgear
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Initial Fire Only" /V
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Cabinet fire in Switchgear" /V
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Combined Fires" /V
cd ..\
cd "3 - Lube Oil Fire in Pump Room"
rem echo 3 - Lube Oil Fire in Pump Room
rem "..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Oil Fire in Pump Room" /V
cd ..\
cd "4 - MCC Fire in Switchgear"
echo 4 - MCC Fire in Switchgear
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "MCC in Switchgear" /V
cd ..\
cd "5 - Trash Fire in Cable Spreading Room"
echo 5 - Trash Fire in Cable Spreading Room
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Trash Fire in Cable Spreading Room" /V
cd ..\
cd "6 - Oil Fire in Turbine Building"
rem echo 6 - Oil Fire in Turbine Building
rem "..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Oil in Turbine Building" /V
cd ..\
cd "7 - Transient Fire in Corridor"
rem echo 7 - Transient Fire in Corridor
rem "..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Transient in Corridor" /V
cd ..\
cd "8 - Cable Fire in Annulus"
echo 8 - Cable Fire in Annulus
"..\..\..\Visual Studio 2010\Projects\cfast\Bin\CFAST.exe" "Cable in Annulus" /V
cd ..\


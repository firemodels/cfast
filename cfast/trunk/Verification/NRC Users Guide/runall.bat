path C:\Documents and Settings\Administrator\My Documents\Visual Studio 2005\Projects\cfast\Bin\; %PATH%
echo off
call cleanall.bat
echo NRC Users Guide Test Cases
cd "1 - Cabinet Fire in MCR"
echo 1 - Cabinet Fire in MCR
cfast "Cabinet fire in MCR" 
cd ..\
cd "2 - Cabinet Fire in Switchgear"
echo 2 - Cabinet Fire in Switchgear
cfast "Cabinet fire in Switchgear" 
cd ..\
cd "3 - Lube Oil Fire in Pump Room"
echo 3 - Lube Oil Fire in Pump Room
cfast "Oil Fire in Pump Room"
cd ..\
cd "4 - MCC Fire in Switchgear"
echo 4 - MCC Fire in Switchgear
cfast "MCC in Switchgear"
cd ..\
cd "5 - Trash Fire in Cable Spreading Room"
echo 5 - Trash Fire in Cable Spreading Room
cfast "Trash Fire in Cable Spreading Room" 
cd ..\
cd "6 - Oil Fire in Turbine Building"
echo 6 - Oil Fire in Turbine Building
cfast "Oil in Turbine Building"
cd ..\
cd "7 - Transient Fire in Corridor"
echo 7 - Transient Fire in Corridor
cfast "Transient in Corridor"
cd ..\

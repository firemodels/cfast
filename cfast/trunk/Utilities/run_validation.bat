@echo off
echo.| time
echo Running validation cases
cd ..\Validation
call runall.bat ALL
call runadjustments.bat
cd ..\Utilities\matlab
Compiled\Validation.exe
cd ..\
echo.| time


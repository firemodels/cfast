@echo off
echo.| time
echo Running validation cases
cd ..\Validation
call runall.bat ALL
call runadjustments.bat
echo Running verification cases
cd ..\Verification
call runall.bat ALL
call runadjustments.bat
echo Running matlab plotting analysis
cd ..\Utilities\matlab
Compiled\Validation.exe
Compiled\Verification.exe
Compiled\Plotting.exe
cd ..\
call make_docs.bat
echo.| time


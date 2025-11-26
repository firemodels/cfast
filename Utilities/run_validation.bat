@echo off
echo.| time
echo Compiling CFAST
cd ..\Source\CFAST\intel_win
del *.obj *.mod *.exe /q
call ..\scripts\setup_intel_compilers.bat intel64
make VPATH="../Source:../Include" INCLUDE="../Include" -f ..\makefile intel_win
copy /Y cfast7_win.exe ..\..\..\bin\cfast.exe
cd ..\..\..\Utilities
echo Running validation cases
Title Running Validation Cases
cd ..\Validation
call runall.bat ALL
call runadjustments.bat
echo Running matlab plotting analysis
cd ..\Utilities\matlab
Title Running Matlab Plotting Analysis
Validation_Script
cd ..\
echo.| time


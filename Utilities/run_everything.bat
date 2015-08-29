@echo off
echo.| time
echo Compiling CFAST
cd ..\CFAST\intel_win_64
del *.obj *.mod *.exe /q
call ..\scripts\setup_intel_compilers.bat intel64
make VPATH="../Source:../Include" INCLUDE="../Include" -f ..\makefile intel_win_64
copy /Y cfast7_win_64.exe ..\..\bin\cfast.exe
cd ..\..\Utilities
echo Running validation cases
Title Running Validation Cases
cd ..\Validation
call runall.bat ALL
call runadjustments.bat
echo Running verification cases
Title Running Verification Cases
cd ..\Verification
call runall.bat ALL
echo Running matlab plotting analysis
cd ..\Utilities\matlab\scripts
SpeciesMassTestCases
cd ..\
Title Running Matlab Plotting Analysis
Validation
Verification
echo Creating Documentation
Title Creating Documentation
cd ..\
call make_docs.bat
echo.| time


@echo off
echo.| time
echo Running validation cases
Title Running Validation Cases
cd ..\Validation
call runall.bat ALL
call runvv.bat
echo Running verification cases
Title Running Verification Cases
cd ..\Verification
call runall.bat ALL
echo Running matlab plotting analysis
cd ..\Utilities\matlab
Title Running Matlab Plotting Analysis
Validation_Script
Verification_Script
echo Creating Documentation
Title Creating Documentation
cd ..\
call make_docs.bat
echo.| time


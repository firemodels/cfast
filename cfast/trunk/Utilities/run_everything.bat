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
call run_matlab_validation.bat
call run_matlab_verification.bat
cd ..\
echo Make Technical Reference Guide
cd ..\Docs\Tech_Ref
pdflatex -interaction nonstopmode $ Tech_Ref > $doc.err
bibtex Tech_Ref >> $doc.err
pdflatex -interaction nonstopmode Tech_Ref >> Tech_Ref.err
pdflatex -interaction nonstopmode Tech_Ref >> Tech_Ref.err
echo.| time


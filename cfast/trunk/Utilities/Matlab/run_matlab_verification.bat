rem @echo  off

set matlab="C:\Program Files\MATLAB\R2013b\bin\matlab"

rem %matlab% -r CFAST_verification_script
rem %matlab% -r CFAST_plotting_script
matlab -automation -wait -noFigureWindows -r "try; run('%CD%\CFAST_verification_script.m'); catch; end; quit
matlab -automation -wait -noFigureWindows -r "try; run('%CD%\CFAST_plotting_script.m'); catch; end; quit
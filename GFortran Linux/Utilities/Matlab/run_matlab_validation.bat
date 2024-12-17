@echo  off

set matlab="C:\Program Files\MATLAB\R2013b\bin\matlab"

Rem %matlab% -r CFAST_validation_script
matlab -automation -wait -noFigureWindows -r "try; run('%CD%\CFAST_validation_script'); catch; end; quit

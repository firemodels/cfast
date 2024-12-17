@echo off

call %SVNROOT%\Validation\Scripts\getopts.bat %*

set fulldir=%BASEDIR%\%dir%


set out=%infile%.out

cd %fulldir%
if exist %out% goto end_of_script
echo ***Error: file %out% not found in %fulldir%

:end_of_script

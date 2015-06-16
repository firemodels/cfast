@echo off

call %cfastroot%\Validation\Scripts\getopts.bat %*

set fulldir=%BASEDIR%/%dir%

cd %fulldir%
echo %infile%
%smokeview% -runscript %infile%

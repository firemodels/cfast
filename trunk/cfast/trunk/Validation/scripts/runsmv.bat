@echo off

call %cfastsvnroot%\Validation\Scripts\getopts.bat %*

set fulldir=%BASEDIR%/%dir%

cd %fulldir%
echo %infile%
%SMOKEVIEW% -runscript %infile%

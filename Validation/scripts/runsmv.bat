@echo off

call %cfastrepo%\Validation\Scripts\getopts.bat %*

set fulldir=%BASEDIR%/%dir%

cd %fulldir%
echo fulldir=%fulldir%
echo infile=%infile%
echo smokeview=%SMOKEVIEW%
%SMOKEVIEW% -runscript %infile%

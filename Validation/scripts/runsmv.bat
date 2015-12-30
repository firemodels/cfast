@echo off

call %cfastroot%\Validation\Scripts\getopts.bat %*

set fulldir=%BASEDIR%/%dir%

cd %fulldir%
echo fulldir=%fulldir%
echo infile=%infile%
echo smokeview=%smokeview%
%smokeview% -runscript %infile%

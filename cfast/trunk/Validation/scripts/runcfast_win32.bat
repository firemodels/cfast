@echo off

call %SVNROOT%\Validation\scripts\getopts.bat %*

set fulldir=%BASEDIR%/%dir%

set in=%infile%
set out=%infile%.err
set stopfile=%infile%.stop

cd %fulldir%
echo starting %in% in %fulldir%
%CFAST% %in%  

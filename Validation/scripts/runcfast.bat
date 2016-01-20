@echo off

call %SVNROOT%\Validation\Scripts\getopts.bat %*

set fulldir=%BASEDIR%/%dir%


set in=%infile%
set out=%infile%.err
set stopfile=%infile%.stop

cd %fulldir%
echo %in% started using %CFAST%
%CFAST% %in%  1> %out% 2>&1 -V

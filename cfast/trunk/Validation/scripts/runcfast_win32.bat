@echo off

:: need to define GETOPTS if this script is called outside of cfastbot
call %GETOPTS% %*

set fulldir=%BASEDIR%/%dir%

set in=%infile%
set out=%infile%.err
set stopfile=%infile%.stop

cd %fulldir%
echo starting %in% in %fulldir%
%CFAST% %in%  

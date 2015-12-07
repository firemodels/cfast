@echo off

echo Creating figures for the CFAST Users guide

set SCRIPT_DIR=%CD%
set BASEDIR=%CD%\..
set SVNROOT=%BASEDIR%\..\
set RUNSMV=call %SVNROOT%\Validation\scripts\runsmv.bat

set SMOKEVIEW="smokeview"

cd %SCRIPT_DIR%
%SCRIPT_DIR%\sh2bat CFAST_Pictures.sh CFAST_Pictures.bat

echo Generating images
cd %BASEDIR%
call %SCRIPT_DIR%\CFAST_Pictures.bat

cd %SCRIPT_DIR%

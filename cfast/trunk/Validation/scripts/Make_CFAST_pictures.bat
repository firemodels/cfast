@echo off

echo Creating figures for the CFAST Users guide

set SCRIPT_DIR=%CD%
set BASEDIR=%CD%\..
set SVNROOT=%BASEDIR%\..\
set RUNCFAST=call %SVNROOT%\Verification\scripts\runsmv_win32.bat

set SMOKEVIEW="smokeview"

cd %SCRIPT_DIR%
%SCRIPT_DIR%\sh2bat CFAST_Cases.sh CFAST_Cases.bat

echo Generating images
cd %BASEDIR%
call %SCRIPT_DIR%\CFAST_Cases.bat

cd %SCRIPT_DIR%

@echo off
set svn_drive=c:

set SCRIPT_DIR=%CD%
set BASEDIR=%CD%\..
set SVNROOT=%BASEDIR%\..\
set TIME_FILE=%SCRIPT_DIR%\smv_case_times.txt

set CFASTEXE=%SVNROOT%\CFAST\intel_win_32\cfast6_win_32
set CFAST=%CFASTEXE%
set RUNCFAST=call %SVNROOT%\Verification\scripts\runcfast_win32.bat

echo You are about to run the CFAST Verification Test Suite.
echo Press any key to proceed, CTRL c to abort
pause > Nul

echo creating CFAST case list from CFAST_Cases.sh
%SCRIPT_DIR%\sh2bat CFAST_Cases.sh CFAST_Cases.bat

cd %BASEDIR%

echo "CFAST test cases begin" > %TIME_FILE%
date /t >> %TIME_FILE%
time /t >> %TIME_FILE%

call %SCRIPT_DIR%\CFAST_Cases.bat

cd %BASEDIR%
echo "CFAST test cases end" >> %TIME_FILE%
date /t >> %TIME_FILE%
time /t >> %TIME_FILE%

erase %SCRIPT_DIR%\CFAST_Cases.bat
cd %SCRIPT_DIR%
echo "CFAST cases completed"

pause

@echo off
set svn_drive=c:

set SCRIPT_DIR=%CD%

cd %CD%\..
set BASEDIR=%CD%

cd "%BASEDIR%\.."
set SVNROOT=%CD%


cd "%SCRIPT_DIR%
set TIME_FILE="%SCRIPT_DIR%\smv_case_times.txt"
set SH2BAT="%SCRIPT_DIR%\sh2bat.exe"

set BACKGROUNDDIR=%SVNROOT%\Validation\scripts\
cd "%BACKGROUNDDIR%"
set BACKGROUNDEXE="%CD%"\background.exe
set bg=%BACKGROUNDEXE% -u 85 -d 1

cd %SVNROOT%\CFAST\intel_win_64
set CFASTEXE=%CD%\cfast6_win_64

cd "%SCRIPT_DIR%"

set CFAST=%bg% %CFASTEXE%
set RUNCFAST=call "%SVNROOT%\Validation\scripts\runcfast_win32.bat"
set RUNCFAST2=call "%SVNROOT%\Validation\scripts\runcfast2_win32.bat"

echo You are about to run the CFAST Validation Test Suite.
echo Press any key to proceed, CTRL c to abort
pause > Nul

echo creating CFAST case list from CFAST_Cases.sh
%SH2BAT% CFAST_Cases.sh CFAST_Cases.bat

cd "%BASEDIR%"

echo "CFAST test cases begin" > %TIME_FILE%
date /t >> %TIME_FILE%
time /t >> %TIME_FILE%

call "%SCRIPT_DIR%\CFAST_Cases.bat"

cd "%BASEDIR%"
echo "CFAST test cases end" >> %TIME_FILE%
date /t >> %TIME_FILE%
time /t >> %TIME_FILE%

erase %SCRIPT_DIR%\CFAST_Cases.bat
cd "%SCRIPT_DIR%"
echo "CFAST cases completed"

pause

@echo off

set SCRIPT_DIR=%CD%

cd %CD%\..
set BASEDIR=%CD%

cd %BASEDIR%\..
set SVNROOT=%CD%

set SH2BAT=%SCRIPT_DIR%\sh2bat.exe
set BACKGROUNDEXE=%SCRIPT_DIR%\\background.exe
set bg=%BACKGROUNDEXE% -u 85 -d 1
set CFASTEXE=%SVNROOT%\CFAST\intel_win_64\cfast7_win_64
set CFAST=%bg% %CFASTEXE%
set RUNCFAST=call %SCRIPT_DIR%\runcfast.bat

cd "%SCRIPT_DIR%"
%SH2BAT% CFAST_Cases.sh CFAST_Cases.bat

cd "%BASEDIR%"
call Scripts\CFAST_Cases.bat

:loop1
tasklist | find /i /c "CFAST" > temp.out
set /p numexe=<temp.out
if %numexe% == 0 goto finished
Timeout /t 30 >nul 
goto loop1

:finished

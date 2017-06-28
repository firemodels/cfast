@echo off

set rundebug=%1
set bgexe=%2
set SH2BAT=%3

if "%bgexe%" == "" (
  set bgexe=background.exe
)

if "%SH2BAT%" == "" (
  set SH2BAT=sh2bat.exe
)

if "%rundebug%" == "1" (
set DEBUG=_db
) else (
set DEBUG=
)

set size=_64

set SCRIPT_DIR=%CD%

cd %CD%\..
set BASEDIR=%CD%

cd %BASEDIR%\..
set SVNROOT=%CD%

set bg=%bgexe% -u 85 -d 0.1
set CFASTEXE=%SVNROOT%\Build\CFAST\intel_win%size%%DEBUG%\cfast7_win%size%%DEBUG%
set CFAST=%bg% %CFASTEXE%

set RUNCFAST_R=call %SCRIPT_DIR%\runcfast.bat
set RUNCFAST_M=call %SCRIPT_DIR%\make_stop.bat
set RUNCFAST_E=call %SCRIPT_DIR%\erase_stop.bat

cd "%SCRIPT_DIR%"
%SH2BAT% CFAST_Cases.sh CFAST_Cases.bat

:: create or erase stop files

if "%rundebug%" == "1" (
  SET RUNCFAST=%RUNCFAST_M%
) else (
  SET RUNCFAST=%RUNCFAST_E%
)
cd "%BASEDIR%"
call Scripts\CFAST_Cases.bat

:: run cases

SET RUNCFAST=%RUNCFAST_R%
cd "%BASEDIR%"
call Scripts\CFAST_Cases.bat

:loop1
tasklist | find /i /c "CFAST" > temp.out
set /p numexe=<temp.out
if %numexe% == 0 goto finished
Timeout /t 30 >nul 
goto loop1

:finished

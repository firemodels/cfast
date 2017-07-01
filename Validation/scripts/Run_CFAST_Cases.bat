@echo off

if exist .valscriptdir goto in_right_dir
   echo ***Error: this script must be run in the Validation\scripts directory
   exit /b
:in_right_dir

set rundebug=0
set installed_cfast=0
set installed_smokeview=0
set size=_64
set DEBUG=

call :getopts %*
if %stopscript% == 1 (
  exit /b
)

set SCRIPT_DIR=%CD%

cd %CD%\..
set BASEDIR=%CD%

cd %BASEDIR%\..
set SVNROOT=%CD%
echo SVNROOT=%SVNROOT%

cd ..\smv
set SMVROOT=%CD%

:: use installed programs

cd %SCRIPT_DIR%
if "%installed_cfast%" == "1" (
   set CFASTEXE=cfast.exe
   call :is_file_installed %CFASTEXE% || exit /b 1
   echo %CFASTEXE% found
)

if "%installed_smokeview%" == "1" (
   set bgexe=background.exe
   call :is_file_installed %bgexe% || exit /b 1
   echo %bgexe% found

   set SH2BAT=sh2bat.exe
   call :is_file_installed %SH2BAT% || exit /b 1
   echo %SH2BAT% found
)

:: use programs from repo

if "%installed_cfast%" == "0" (
  cd %SCRIPT_DIR%
  
  set CFASTEXE=%SVNROOT%\Build\CFAST\intel_win%size%%DEBUG%\cfast7_win%size%%DEBUG%.exe
  call :does_file_exist %CFASTEXE% || exit /b 1
  echo %CFASTEXE% found
)

if "%installed_smokeview%" == "0" (
  cd %SCRIPT_DIR%

  set bgexe=%SMVROOT%\Build\background\intel_win%size%\background.exe
  call :does_file_exist %bgexe% || exit /b 1
  echo %bgexe% found

  set SH2BAT=%SMVROOT%\Build\sh2bat\intel_win%size%\sh2bat_win%size%.exe
  call :does_file_exist %SH2BAT% || exit /b 1
  echo %SH2BAT% found
)

set bg=%bgexe% -u 85 -d 0.1
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
echo waiting for %numexe% jobs to finish
if %numexe% == 0 goto finished
Timeout /t 30 >nul 
goto loop1

:finished

goto eof

:: -------------------------------------------------------------
:is_file_installed
:: -------------------------------------------------------------

  set program=%1
  %program% -help 1> installed_error.txt 2>&1
  type installed_error.txt | find /i /c "not recognized" > installed_error_count.txt
  set /p nothave=<installed_error_count.txt
  erase installed_error_count.txt installed_error.txt
  if %nothave% == 1 (
    echo "***Fatal error: %program% not present"
    exit /b 1
  )
  exit /b 0

:: -------------------------------------------------------------
  :does_file_exist
:: -------------------------------------------------------------

set file=%1

if NOT exist %file% (
  echo ***Fatal error: %file% does not exist. Aborting
  exit /b 1
)
exit /b 0

:getopts
 set stopscript=0
 if (%1)==() exit /b
 set valid=0
 set arg=%1
 if /I "%1" EQU "-debug" (
   set valid=1
   set rundebug=1
   set DEBUG=_db
   exit /b
 )
 if /I "%1" EQU "-help" (
   call :usage
   set stopscript=1
   exit /b
 )
 if /I "%1" EQU "-cfast" (
   set valid=1
   set installed_cfast=1
 )
 if /I "%1" EQU "-smokeview" (
   set valid=1
   set installed_smokeview=1
 )
 if /I "%1" EQU "-installed" (
   set valid=1
   set installed_smokeview=1
   set installed_cfast=1
 )
 shift
 if %valid% == 0 (
   echo.
   echo ***Error: the input argument %arg% is invalid
   echo.
   echo Usage:
   call :usage
   set stopscript=1
   exit /b
 )
if not (%1)==() goto getopts
exit /b

:usage  
echo Run_CFAST_Cases [options]
echo. 
echo -cfast          - use installed cfast
echo -debug          - use debug version of cfast
echo -help           - display this message
echo -installed      - same as -cfast -smokeview
echo -smokeview      - use installed smokeview utilities 
echo                   (background and sh2bat)
exit /b

:eof
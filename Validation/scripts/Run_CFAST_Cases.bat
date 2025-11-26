@echo off

if exist .valscriptdir goto in_right_dir
   echo ***Error: this script must be run in the Validation\scripts directory
   exit /b
:in_right_dir

set rundebug=0
set installed_cfast=0
set installed_smokeview=0
set DEBUG=
set check=

call :getopts %*
if %stopscript% == 1 (
  exit /b
)

set SCRIPT_DIR=%CD%

cd %CD%\..
set BASEDIR=%CD%

cd %BASEDIR%\..
set SVNROOT=%CD%

::*** use installed programs

cd %SCRIPT_DIR%
if "%installed_cfast%" == "0" goto not_installed_cfast
   set CFAST_EXE=cfast.exe
   call :is_file_installed %CFAST_EXE% || exit /b 1
   echo %CFAST_EXE% found
:not_installed_cfast

if "%installed_smokeview%" == "0" goto skip_not_installed_smokeview
   set bgexe=background.exe
   call :is_file_installed %bgexe% || exit /b 1
   echo %bgexe% found

   set SH2BAT=sh2bat.exe
   call :is_file_installed %SH2BAT% || exit /b 1
   echo %SH2BAT% found
:skip_not_installed_smokeview

::*** use programs from repo

if "%installed_cfast%" == "1" goto skip_installed_cfast
  cd %SCRIPT_DIR%

  set CFAST_EXE=%SVNROOT%\Build\CFAST\intel_win%DEBUG%\cfast7_win%DEBUG%.exe
  call :does_fortfile_exist %CFAST_EXE% || exit /b 1
  echo %CFAST_EXE% found
:skip_installed_cfast

if "%installed_smokeview%" == "1" goto skip_installed_smokeview
  cd %SCRIPT_DIR%

  if NOT EXIST ..\..\..\smv goto abort_smv
  cd ..\..\..\smv
  set SMVROOT=%CD%

  cd %SCRIPT_DIR%

  set bgexe=%SMVROOT%\Build\background\intel_win\background.exe
  call :does_cfile_exist %bgexe% || exit /b 1
  echo %bgexe% found

  set SH2BAT=%SMVROOT%\Build\sh2bat\intel_win\sh2bat_win.exe
  call :does_cfile_exist %SH2BAT% || exit /b 1
  echo %SH2BAT% found
:skip_installed_smokeview

set bg=%bgexe% -u 85 -d 0.1
set CFAST=%bg% %CFAST_EXE%

set RUNCFAST_R=call %SCRIPT_DIR%\runcfast.bat
set RUNCFAST_M=call %SCRIPT_DIR%\make_stop.bat
set RUNCFAST_E=call %SCRIPT_DIR%\erase_stop.bat
set RUNCFAST_C=call %SCRIPT_DIR%\checkcfast.bat

cd "%SCRIPT_DIR%"
%SH2BAT% CFAST_Cases.sh CFAST_Cases.bat

:: create or erase stop files

cd "%BASEDIR%"
if "%check%" == "1" goto skip1
if "%rundebug%" == "1" (
  SET RUNCFAST=%RUNCFAST_M%
  call Scripts\CFAST_Cases.bat
) else (
  SET RUNCFAST=%RUNCFAST_E%
  call Scripts\CFAST_Cases.bat
)
:skip1

:: run cases

SET RUNCFAST=%RUNCFAST_R%
if "%check%" == "1" (
  SET RUNCFAST=%RUNCFAST_C%
)
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
cd %SCRIPT_DIR%

goto eof

:: -------------------------------------------------------------
:is_file_installed
:: -------------------------------------------------------------

  set program=%1
  where %program% 1> installed_error.txt 2>&1
  type installed_error.txt | find /i /c "Could not find" > installed_error_count.txt
  set /p nothave=<installed_error_count.txt
  erase installed_error_count.txt installed_error.txt
  if %nothave% == 1 (
    echo "***Fatal error: %program% not present"
    exit /b 1
  )
  exit /b 0

:: -------------------------------------------------------------
  :does_fortfile_exist
:: -------------------------------------------------------------

set file=%1

if NOT exist %file% (
  echo ***Fatal error: The Fortran program %file% does not exist.
  echo Either build this program or rerun Run_CFAST_Cases.bat using
  echo the -cfast option
  exit /b 1
)
exit /b 0

:: -------------------------------------------------------------
  :does_cfile_exist
:: -------------------------------------------------------------

set file=%1

if NOT exist %file% (
  echo ***Fatal error: The C/C++ program %file% does not exist.
  echo Either build this program or rerun Run_CFAST_Cases.bat using the
  echo -smokeview option
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
 )
 if /I "%1" EQU "-help" (
   call :usage
   set stopscript=1
   exit /b
 )
 if /I "%1" EQU "-check" (
   set valid=1
   set check=1
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
echo -check          - check that cases ran (that .out file was generated)
echo -help           - display this message
echo -installed      - same as -cfast -smokeview
echo -smokeview      - use installed smokeview utilities 
echo                   (background and sh2bat)
exit /b

:abort_smv
echo ***Fatal error: the smv repo does not exist
echo clone the smv repo at the same level as the cfast repo
echo or rerun Run_CFAST_cases.bat using the -smokeview option
exit /b
goto eof


:eof

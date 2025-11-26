@echo off

echo Creating figures for the CFAST Users guide

set installed_smokeview=0

call :getopts %*
if %stopscript% == 1 (
  exit /b
)

set SCRIPT_DIR=%CD%
cd ..
set BASEDIR=%CD%
cd ..
set SVNROOT=%CD%

cd %SCRIPT_DIR%

set RUNSMV=call %SVNROOT%\Validation\scripts\runsmv.bat

if "%installed_smokeview%" == "0" goto skip_installed_smokeview
   set SMOKEVIEW=smokeview.exe
   call :is_file_installed %SMOKEVIEW% || exit /b 1
   echo %SMOKEVIEW% found

   set SH2BAT=sh2bat.exe
   call :is_file_installed %SH2BAT% || exit /b 1
   echo %SH2BAT% found
:skip_installed_smokeview

if "%installed_smokeview%" == "1" goto installed_smokeview
  cd %SCRIPT_DIR%

  if NOT EXIST ..\..\..\smv goto abort_smv
  cd ..\..\..\smv
  set SMVROOT=%CD%

  cd %SCRIPT_DIR%

  set SMOKEVIEW=%SMVROOT%\Build\smokeview\intel_win\smokeview_win.exe
  call :does_cfile_exist %SMOKEVIEW% || exit /b 1
  echo %SMOKEVIEW% found

  set SH2BAT=%SMVROOT%\Build\sh2bat\intel_win\sh2bat_win.exe
  call :does_cfile_exist %SH2BAT% || exit /b 1
  echo %SH2BAT% found
:installed_smokeview

cd %SCRIPT_DIR%
%SH2BAT% CFAST_Pictures.sh CFAST_Pictures.bat

echo Generating images
cd %BASEDIR%
call %SCRIPT_DIR%\CFAST_Pictures.bat

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
  :does_cfile_exist
:: -------------------------------------------------------------

set file=%1

if NOT exist %file% (
  echo ***Fatal error: The C/C++ program %file% does not exist.
  echo Either build this program or rerun Make_CFAST_Pictures.bat using the
  echo -smokeview option
  exit /b 1
)
exit /b 0

:getopts
 set stopscript=0
 if (%1)==() exit /b
 set valid=0
 set arg=%1
 if /I "%1" EQU "-help" (
   call :usage
   set stopscript=1
   exit /b
 )
 if /I "%1" EQU "-smokeview" (
   set valid=1
   set installed_smokeview=1
 )
 if /I "%1" EQU "-installed" (
   set valid=1
   set installed_smokeview=1
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
echo -help           - display this message
echo -smokeview      - use installed smokeview and utilities 
exit /b

:abort_smv
echo ***Fatal error: the smv repo does not exist
echo clone the smv repo at the same level as the cfast repo
echo or rerun Make_CFAST_pictures.bat using the -smokeview option
exit /b
goto eof

:eof

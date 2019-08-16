@echo off
set script_dir=%~dp0

title Smokeview Installer

:: before we do anything make sure this is a 64 bit PC

if defined PROGRAMFILES(X86) (
  echo.
) else (
  echo.
  echo *** Fatal error: 32 bit Windows detected.
  echo     Smokeview can only run on 64 bit systems.
  echo     Installation aborted.
  echo *** Press any key to continue.    ***
  pause>NUL
  goto abort
)

:quest1
set auto_install=y
type message.txt
echo.
echo Install in %SystemDrive%\Program Files\firemodels and stop any instances of smokeview?
echo.
echo  yes - standard installation (use default answer for all options that follow)
echo   no - customize installation (install to a different location)
echo quit - stop installation
echo.
set /p  auto_install="yes, no or quit?:"
call :get_yesno %auto_install% auto_install quest1_repeat
if "%quest1_repeat%" == "1" goto quest1
if "%quest1_repeat%" == "2" goto abort

::*** check if smokeview is running

:begin
set progs_running=0
call :count_programs

if "%progs_running%" == "0" goto start
  if "%auto_install%" == "y" goto stop_smokeview
  echo smokeview needs to be stopped before proceeding with the installation
  echo Options:
  echo   Press 1 to stop smokeview (default: 1) 
  echo   Press any other key to quit installation
:stop_smokeview

  set option=1
  if "%auto_install%" == "n" set /p  option="Option:"
  if "%option%" == "1" (
    call :stop_smokeview
    goto start
  )
  goto abort

::*** determine install directory

:start

if "%auto_install%" == "y" goto install_location
echo.
type message.txt
echo.
echo Options:
echo    Press 1 to install for all users (default: 1)
echo    Press 2 to install for user %USERNAME%
echo    Press any other key to cancel the installation
:install_location

set option=1
if "%auto_install%" == "n" set /p option="Option:"

set option_install=0
if "%option%" == "1" set option_install=1
if "%option%" == "2" set option_install=2
if "%option_install%" == "0" goto abort

set "BASEDIR=%SystemDrive%\Program Files"
if "%option_install%" == "2" set "BASEDIR=%userprofile%"

set subdir=firemodels
set "INSTALLDIR=%BASEDIR%\%subdir%"
echo.
if "%auto_install%" == "n" set /p INSTALLDIR="Enter FDS/Smokeview root directory (default: %INSTALLDIR%):"

::*** start Smokeview installation

:install

echo.
echo Installation directory: %INSTALLDIR%
echo.

set "SMV6=%INSTALLDIR%\SMV6"

set need_overwrite=0
if EXIST "%SMV6%" set need_overwrite=1

if "%need_overwrite%" == "0" goto else1 
  echo The directory %subdir%\SMV6 exists. 
  set option=n
  if "%auto_install%" == "y" set option=y  
  if "%auto_install%" == "n" set /p option="Do you wish to overwrite it? (yes, no (default: no)):"
  goto endif1
:else1
  set option=y
  if "%auto_install%" == "n" set /p option="Do you wish to proceed? (yes, no, (default: yes)):"
:endif1

set option=%option:~0,1%
if "x%option%" == "xy" goto proceed
if "x%option%" == "xY" goto proceed
goto begin

:proceed

echo.

if NOT exist "%SMV6%" goto skip_remove_smv6
echo *** Removing %SMV6%
rmdir /S /Q "%SMV6%"
:skip_remove_smv6

:: copy files to new installation

echo.
echo *** Copying installation files to %INSTALLDIR%
if NOT EXIST "%INSTALLDIR%" mkdir "%INSTALLDIR%" > Nul
xcopy /E /I /H /Q SMV6 "%SMV6%"     > Nul
echo        copy complete

echo *** Removing previous Smokeview entries from the system and user path.
call "%SMV6%\set_path.exe" -u -m -b -r "firemodels\SMV6" >Nul
call "%SMV6%\set_path.exe" -s -m -b -r "firemodels\SMV6" >Nul

echo *** Setting up PATH variable.

if NOT "%option_install%" == "1" goto skip_systempath
  call "%SMV6%\set_path.exe" -s -m -f "%SMV6%"     > Nul
  goto after_setpath
:skip_systempath

call "%SMV6%\set_path.exe" -u -m -f "%SMV6%"     > Nul
:after_setpath

:: ------------- file association -------------
echo *** Associating the .smv file extension with smokeview.exe

ftype smvDoc="%SMV6%\smokeview.exe" "%%1" >Nul
assoc .smv=smvDoc>Nul


echo.
echo *** Press any key, then reboot to complete the installation.  ***
pause>NUL
goto eof

:-------------------------------------------------------------------------
:----------------------subroutines----------------------------------------
:-------------------------------------------------------------------------

:-------------------------------------------------------------------------
:get_yesno  
:-------------------------------------------------------------------------
set answer=%1
set answervar=%2
set repeatvar=%3

set answer=%answer:~0,1%
if "%answer%" == "Y" set answer=y
if "%answer%" == "N" set answer=n
if "%answer%" == "S" set answer=s
set %answervar%=%answer%
set repeat=1
if "%answer%" == "y" set repeat=0
if "%answer%" == "n" set repeat=0
if "%answer%" == "s" set repeat=2
set %repeatvar%=%repeat%
exit /b

:-------------------------------------------------------------------------
:count_programs  
:-------------------------------------------------------------------------
call :count smokeview
exit /b

:-------------------------------------------------------------------------
:stop_smokeview  
:-------------------------------------------------------------------------
:: remove old installation

if NOT "%smokeview_count%" == "0" (
  echo *** Stopping smokeview
  taskkill /F /IM smokeview.exe >Nul 2>Nul
)
exit /b

:-------------------------------------------------------------------------
:count
:-------------------------------------------------------------------------
set progbase=%1
set prog=%progbase%.exe
set countvar=%progbase%_count
set stringvar=%progbase%_string

tasklist | find /c "%prog%" > count.txt
set /p count%=<count.txt
erase count.txt

set string=
if NOT "%count%" == "0" set string=%progbase%
if NOT "%count%" == "0" set progs_running=1

set %countvar%=%count%
set %stringvar%=%string%

exit /b

:abort
echo Smokeview installation aborted.
echo Press any key to finish
pause > Nul

:eof
exit
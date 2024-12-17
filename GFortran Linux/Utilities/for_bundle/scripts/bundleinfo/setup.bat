@echo off
set script_dir=%~dp0

title Cfast Installer

:: before we do anything make sure this is a 64 bit PC

if defined PROGRAMFILES(X86) (
  echo.
) else (
  echo.
  echo *** Fatal error: 32 bit Windows detected.
  echo     Cfast can only run on 64 bit systems.
  echo     Installation aborted.
  echo *** Press any key to continue.    ***
  pause>NUL
  goto abort
)

:: form extraction directory

set /p basename=<firemodels\basename.txt
set EXTRACTDIR=%userprofile%\%basename%
for /f "tokens=* delims= " %%A in ('echo %EXTRACTDIR% ') do set EXTRACTDIR=%%A
set EXTRACTDIR=%EXTRACTDIR:~0,-1%

:quest1
set auto_install=y
type firemodels\message.txt
echo.
echo Install in %SystemDrive%\Program Files\firemodels and stop any 
echo instances of cfast or smokeview?
echo.
echo  yes - standard installation (use default answer for all questions)
echo   no - customize installation (install to a different location)
::echo extract - extract to: %EXTRACTDIR%
::echo           and quit 
echo quit - stop installation
echo.
set /p  auto_install="yes, no or quit?:"
call :get_yesnoextract %auto_install% auto_install quest1_repeat
if "%quest1_repeat%" == "1" goto quest1
if "%quest1_repeat%" == "2" goto abort
if "%quest1_repeat%" == "3" goto extract

::*** check if cfast and smokeview are running

:begin
set progs_running=0
call :count_programs

if "%progs_running%" == "0" goto start
  if "%auto_install%" == "y" goto skip_remove
  echo The following program(s) need to be stopped before proceeding with the installation:
  echo %cfast_string% %smokeview_string% 
  echo.
  echo Options:
  echo   Press 1 to stop these programs (default: 1) 
  echo   Press any other key to quit installation
  :skip_remove

  set option=1
  if "%auto_install%" == "n" set /p  option="Option:"
  if "%option%" == "1" (
    call :stop_programs
    goto start
  )
  goto abort

::*** determine install directory

:start

if "%auto_install%" == "y" goto skip_loc
echo.
type firemodels\message.txt
echo.
echo Options:
echo    Press 1 to install for all users (default: 1)
echo    Press 2 to install for user %USERNAME%
echo    Press any other key to cancel the installation
:skip_loc

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
if "%auto_install%" == "n" set /p INSTALLDIR="Enter Cfast root directory (default: %INSTALLDIR%):"

::*** start Cfast installation

:install

echo.
echo Installation directory: %INSTALLDIR%
echo.

set "SMV6=%INSTALLDIR%\SMV6"
set "CFAST7=%INSTALLDIR%\cfast"
set "UNINSTALLDIR=%CFAST7%\Uninstall"

set need_overwrite=0
if EXIST "%CFAST7%" set need_overwrite=1 
if EXIST "%SMV6%" set need_overwrite=1

:quest2
if "%need_overwrite%" == "0" goto else1 
  if "%auto_install%" == "n" echo The directories %subdir%\FDS6 and/or %subdir%\SMV6 exist. 
  set option=n
  if "%auto_install%" == "y" set option=y 
  if "%auto_install%" == "n" set /p option="Do you wish to overwrite them? (yes, no (default: no)):"
  goto endif1
:else1
  set option=y
  if "%auto_install%" == "n" set /p option="Do you wish to proceed? (yes, no, (default: yes)):"
:endif1

set option=%option:~0,1%
call :get_yesno %option% option quest2_repeat
if "%quest2_repeat%" == "1" goto quest2

if "x%option%" == "xy" goto proceed
goto begin

:proceed

if NOT exist "%CFAST7%" goto skip_remove_fds6
echo *** Removing %CFAST7%
rmdir /S /Q "%CFAST7%"
:skip_remove_fds6

if NOT exist "%SMV6%" goto skip_remove_smv6
echo *** Removing %SMV6%
rmdir /S /Q "%SMV6%"
:skip_remove_smv6

:: copy files to new installation

echo.
echo *** Copying installation files to %INSTALLDIR%
if NOT EXIST "%INSTALLDIR%" mkdir "%INSTALLDIR%" > Nul
xcopy /E /I /H /Q firemodels\cfast "%CFAST7%"     > Nul
xcopy /E /I /H /Q firemodels\SMV6  "%SMV6%"     > Nul

set "filepath=%CFAST7%\\cfast7.exe%"
call :is_file_copied cfast7.exe

set "filepath=%SMV6%\smokeview.exe"
call :is_file_copied smokeview.exe

echo        copy complete

echo *** Removing previous cfast entries from the system and user path.
call "%UNINSTALLDIR%\set_path.exe" -s -m -b -r "nist\fds" >Nul
call "%UNINSTALLDIR%\set_path.exe" -u -m -b -r "FDS\FDS5" >Nul
call "%UNINSTALLDIR%\set_path.exe" -s -m -b -r "FDS\FDS5" >Nul
call "%UNINSTALLDIR%\set_path.exe" -u -m -b -r "FDS\FDS6" >Nul
call "%UNINSTALLDIR%\set_path.exe" -s -m -b -r "FDS\FDS6" >Nul
call "%UNINSTALLDIR%\set_path.exe" -s -m -b -r "firemodels\FDS6" >Nul
call "%UNINSTALLDIR%\set_path.exe" -s -m -b -r "firemodels\SMV6" >Nul
call "%UNINSTALLDIR%\set_path.exe" -u -m -b -r "firemodels\FDS6" >Nul
call "%UNINSTALLDIR%\set_path.exe" -u -m -b -r "firemodels\SMV6" >Nul
call "%UNINSTALLDIR%\set_path.exe" -u -m -b -r "firemodels\cfast" >Nul

:: ------------ create aliases ----------------

set numcoresfile="%TEMP%\numcoresfile"

:: ------------ setting up path ------------

echo *** Setting up PATH variable.

if NOT "%option_install%" == "1" goto skip_systempath
  call "%UNINSTALLDIR%\set_path.exe" -s -m -f "%CFAST7%\bin" > Nul
  call "%UNINSTALLDIR%\set_path.exe" -s -m -f "%SMV6%"     > Nul
  goto after_setpath
:skip_systempath

call "%UNINSTALLDIR%\set_path.exe" -u -m -f "%CFAST7%\bin" > Nul
call "%UNINSTALLDIR%\set_path.exe" -u -m -f "%SMV6%"     > Nul

:after_setpath

:: ------------- file association -------------
echo *** Associating the .smv file extension with smokeview.exe

ftype smvDoc="%SMV6%\smokeview.exe" "%%1" >Nul
assoc .smv=smvDoc>Nul

set CFASTSTART=%ALLUSERSPROFILE%\Start Menu\Programs\CFAST7

:: ------------- start menu shortcuts ---------------
echo *** Adding document shortcuts to the Start menu.
if exist "%CFASTSTART%" rmdir /q /s "%CFASTSTART%"

mkdir "%CFASTSTART%"

mkdir "%cfaststartmenu%"

mkdir "%cfaststartmenu%\Guides"
"%CFAST7%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Users Guide.lnk"                                     /T:"%CFAST7%\Documents\Users_Guide.pdf"         /A:C >NUL
"%CFAST7%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Technical Reference Guide.lnk"                       /T:"%CFAST7%\Documents\Tech_Ref.pdf"            /A:C >NUL
"%CFAST7%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Software Development and Model Evaluation Guide.lnk" /T:"%CFAST7%\Documents\Validation_Guide.pdf"    /A:C >NUL
"%CFAST7%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Configuration Management.lnk"                        /T:"%CFAST7%\Documents\Configuration_Guide.pdf" /A:C >NUL
"%CFAST7%\shortcut.exe" /F:"%cfaststartmenu%\CFAST7.lnk"                                                       /T:"%CFAST7%\CEdit.exe"                         /A:C >NUL
"%CFAST7%\shortcut.exe" /F:"%cfaststartmenu%\Smokeview.lnk"                                                    /T:"%SMV6%\smokeview.exe"                       /A:C >NUL
"%CFAST7%\shortcut.exe" /F:"%cfaststartmenu%\Uninstall.lnk"                                                    /T:"%CFAST7%\Uninstall\uninstall.bat"           /A:C >NUL

"%CFAST7%\shortcut.exe" /F:"%CFASTSTART%\Uninstall.lnk"  /T:"%UNINSTALLDIR%\uninstall.bat" /A:C >NUL

:: ----------- setting up uninstall file

echo *** Setting up the Uninstall script.

:: remove smokeview path and directory

echo if "%%cfastinstalled%%" == "1" goto skip2                >> "%UNINSTALLDIR%\uninstall_base.bat"
echo echo Removing "%SMV6%" from the System Path              >> "%UNINSTALLDIR%\uninstall_base.bat"
echo call "%UNINSTALLDIR%\set_path.exe" -s -b -r %SMV6%       >> "%UNINSTALLDIR%\uninstall_base.bat"
echo rmdir /s /q "%SMV6%"                                     >> "%UNINSTALLDIR%\uninstall_base.bat"
echo :skip2                                                   >> "%UNINSTALLDIR%\uninstall_base.bat"

:: remove FDS path and directory

echo echo Removing "%FDS6%\bin" from the System Path          >> "%UNINSTALLDIR%\uninstall_base.bat"
echo call "%UNINSTALLDIR%\set_path.exe" -s -b -r "%FDS6%\bin" >> "%UNINSTALLDIR%\uninstall_base.bat"
echo echo.                                                    >> "%UNINSTALLDIR%\uninstall_base.bat"
echo echo Removing "%FDS6%"                                   >> "%UNINSTALLDIR%\uninstall_base.bat"
echo rmdir /s /q  "%FDS6%"                                    >> "%UNINSTALLDIR%\uninstall_base.bat"

:: if cfast exists then only remove fds
:: if cfast does not exist then remove everything

echo if exist "%CFAST7%" goto skip_remove                     >> "%UNINSTALLDIR%\uninstall_base.bat"
echo   echo Removing "CFAST7%"                                >> "%UNINSTALLDIR%\uninstall_base.bat"
echo   rmdir /s /q  "%CFAST7%"                                >> "%UNINSTALLDIR%\uninstall_base.bat"
echo   echo Removing "%INSTALLDIR%"                           >> "%UNINSTALLDIR%\uninstall_base.bat"
echo   rmdir "%INSTALLDIR%"                                   >> "%UNINSTALLDIR%\uninstall_base.bat"
echo :skip_remove                                             >> "%UNINSTALLDIR%\uninstall_base.bat"

echo echo *** Uninstall complete                              >> "%UNINSTALLDIR%\uninstall_base.bat"
echo pause>Nul                                                >> "%UNINSTALLDIR%\uninstall_base.bat"

type  "%UNINSTALLDIR%\uninstall_base2.bat"                    >> "%UNINSTALLDIR%\uninstall_base.bat"
erase "%UNINSTALLDIR%\uninstall_base2.bat"

echo "%UNINSTALLDIR%\uninstall.vbs"                           >> "%UNINSTALLDIR%\uninstall.bat"
echo echo Uninstall complete                                  >> "%UNINSTALLDIR%\uninstall.bat"
echo pause                                                    >> "%UNINSTALLDIR%\uninstall.bat"

set "ELEVATE_APP=%UNINSTALLDIR%\uninstall_base.bat"
set ELEVATE_PARMS=
echo Set objShell = CreateObject("Shell.Application")                       > "%UNINSTALLDIR%\uninstall.vbs"
echo Set objWshShell = WScript.CreateObject("WScript.Shell")               >> "%UNINSTALLDIR%\uninstall.vbs"
echo Set objWshProcessEnv = objWshShell.Environment("PROCESS")             >> "%UNINSTALLDIR%\uninstall.vbs"
echo objShell.ShellExecute "%ELEVATE_APP%", "%ELEVATE_PARMS%", "", "runas" >> "%UNINSTALLDIR%\uninstall.vbs"
echo WScript.Sleep 10000                                                   >> "%UNINSTALLDIR%\uninstall.vbs"

erase "%firewall_setup%"               > Nul
erase "%FDS6%\shortcut.exe"            > Nul

call :is_file_in_path fds
call :is_file_in_path smokeview
call :is_file_in_path mpiexec
echo.
echo To run fds for cases using this computer only, open the
echo command shell CMDfds (located on the desktop) and type:
echo.
echo fds_local casename.fds
echo.
echo where casename is the name of your case. For more information type: helpfds.
echo. 
echo *** Press any key, then reboot to complete the installation.  ***
pause>NUL
goto eof

:-------------------------------------------------------------------------
:----------------------subroutines----------------------------------------
:-------------------------------------------------------------------------

:: -------------------------------------------------------------
:is_file_copied
:: -------------------------------------------------------------

  set file=%1
  if not exist "%filepath%" echo.
  if not exist "%filepath%" echo ***error: %file% failed to copy to %filepath%
  exit /b 0

:: -------------------------------------------------------------
:is_file_in_path
:: -------------------------------------------------------------

  set program=%1
  where %program% 1> %TEMP%\in_path.txt 2>&1
  type %TEMP%\in_path.txt | find /i /c "INFO" > %TEMP%\in_path_count.txt
  set /p nothave=<%TEMP%\in_path_count.txt
  if %nothave% == 1 (
    echo "***Warning: %program% was not found in the PATH."
    echo "   You will need to reboot your computer so that new path entries are defined"
    exit /b 1
  )
  if exist %TEMP%\in_path.txt erase %TEMP%\in_path.txt
  if exist %TEMP%\in_path_count.txt erase %TEMP%\in_path_count.txt
  exit /b 0

:-------------------------------------------------------------------------
:get_yesnoextract
:-------------------------------------------------------------------------
set answer=%1
set answervar=%2
set repeatvar=%3

set answer=%answer:~0,1%
if "%answer%" == "Y" set answer=y
if "%answer%" == "N" set answer=n
if "%answer%" == "Q" set answer=q
if "%answer%" == "E" set answer=e
set %answervar%=%answer%
set repeat=1
if "%answer%" == "y" set repeat=0
if "%answer%" == "n" set repeat=0
if "%answer%" == "q" set repeat=2
if "%answer%" == "e" set repeat=3
set %repeatvar%=%repeat%
exit /b

:-------------------------------------------------------------------------
:get_yesno
:-------------------------------------------------------------------------
set answer=%1
set answervar=%2
set repeatvar=%3

set answer=%answer:~0,1%
if "%answer%" == "Y" set answer=y
if "%answer%" == "N" set answer=n
set %answervar%=%answer%
set repeat=1
if "%answer%" == "y" set repeat=0
if "%answer%" == "n" set repeat=0
set %repeatvar%=%repeat%
exit /b

:-------------------------------------------------------------------------
:count_programs  
:-------------------------------------------------------------------------
call :count cfast
call :count smokeview
exit /b

:-------------------------------------------------------------------------
:stop_programs  
:-------------------------------------------------------------------------
:: remove old installation

if NOT "%smokeview_count%" == "0" (
  echo *** Stopping smokeview
  taskkill /F /IM smokeview.exe >Nul 2>Nul
)

if NOT "%cfast_count%" == "0" (
  echo *** Stopping cfast
  taskkill /F /IM cfast.exe       >Nul 2>Nul
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

:-------------------------------------------------------------------------
:extract
:-------------------------------------------------------------------------
echo.
set "INSTALLDIR=%EXTRACTDIR%"
set "SMV6=%INSTALLDIR%\SMV6"
set "FDS6=%INSTALLDIR%\FDS6"
echo *** Copying installation files to %INSTALLDIR%
if NOT EXIST "%INSTALLDIR%" mkdir "%INSTALLDIR%" > Nul
xcopy /E /I /H /Q firemodels\FDS6 "%FDS6%"     > Nul
xcopy /E /I /H /Q firemodels\SMV6 "%SMV6%"     > Nul

echo Copy complete
echo Press any key to finish
pause > Nul
goto eof

:abort
echo Cfast installation aborted.
echo Press any key to finish
pause > Nul

:eof
exit
@echo off

IF not EXIST placeholder.txt goto dircheck
echo ***error: This script is running in the wrong directory.
pause
exit
:dircheck

echo.
echo *** Wrapping up the cfast installation.
echo.

set SAVECD="%CD%"

cd "%CD%\.."

:: ------------ setting up path ------------

echo.
echo *** Setting up the PATH variable.

:: *** c:\...\cfast\bin
call "%CD%\set_path.exe" -s -m -f "%CD%\bin" >Nul

set cfaststart=%ALLUSERSPROFILE%\Start Menu\Programs\cfast

:: ------------- start menu shortcuts ---------------
echo. 
echo *** Adding document shortcuts to the Start menu.
if exist "%cfaststart%" rmdir /q /s "%cfaststart%"

mkdir "%cfaststart%"

mkdir "%cfaststart%\Guides and Release Notes"
"%CD%\shortcut.exe" /F:"%cfaststart%\Guides and Release Notes\CFAST Technical Reference Guide.lnk"       /T:"%CD%\Documentation\Guides_and_Release_Notes\FDS_Technical_Reference_Guide.pdf" /A:C >NUL

"%CD%\shortcut.exe" /F:"%cfaststart%\Uninstall.lnk"  /T:"%CD%\Uninstall\uninstall.bat" /A:C >NUL

erase "%CD%"\set_path.exe
erase "%CD%"\shortcut.exe


:: ----------- copy backup files to Uninstall directory

copy *.txt Uninstall > Nul
erase /q *.txt

:: ----------- setting up uninstall file

echo.
echo *** Setting up Uninstall script.
echo echo. >> Uninstall\uninstall_base.bat
echo echo Removing directories, %CD%\bin and %SHORTCUTSDIR%, from the System Path >> Uninstall\uninstall_base.bat
echo call "%CD%\Uninstall\set_path.exe" -s -b -r "%CD%\bin" >> Uninstall\uninstall_base.bat
echo call "%CD%\Uninstall\set_path.exe" -s -b -r "%SHORTCUTSDIR%" >> Uninstall\uninstall_base.bat

echo echo. >> Uninstall\uninstall_base.bat
echo echo Removing %CD% >> Uninstall\uninstall_base.bat
echo rmdir /s /q "%SHORTCUTSDIR%" >> Uninstall\uninstall_base.bat
echo rmdir /s /q "%CD%" >> Uninstall\Uninstall_base.bat
echo echo *** Uninstall complete >> Uninstall\uninstall_base.bat
echo pause>Nul >> Uninstall\uninstall_base.bat

echo "%CD%\Uninstall\uninstall.vbs" >> Uninstall\uninstall.bat
echo echo Uninstall complete >> Uninstall\uninstall.bat
echo pause >> Uninstall\uninstall.bat

set ELEVATE_APP=%CD%\Uninstall\Uninstall_base.bat
set ELEVATE_PARMS=
echo Set objShell = CreateObject("Shell.Application") >>Uninstall\uninstall.vbs
echo Set objWshShell = WScript.CreateObject("WScript.Shell") >>Uninstall\uninstall.vbs
echo Set objWshProcessEnv = objWshShell.Environment("PROCESS") >>Uninstall\uninstall.vbs
echo objShell.ShellExecute "%ELEVATE_APP%", "%ELEVATE_PARMS%", "", "runas" >>Uninstall\uninstall.vbs

echo.
echo *** Press any key to complete the installation.
pause>NUL

erase "%CD%"\wrapup_cfast_install.bat >Nul


@echo off
Title Bundle cfast for Windows


:: installation settings settings

set installerbase=cfast7_installer
set distname=cfast7

:: VVVVVVVVVVVVVVVVV shouldn't need to change anything below VVVVVVVVVVVVVVV


set CURDIR=%CD%
cd ..
set git_root=%CD%
cd %CURDIR%
set git_drive=c:

%git_drive%

set DISTDIR=%git_root%\scripts\BUNDLEDIR\%installerbase%
set bundleinfo=%git_root%\scripts\bundleinfo

call Create_Install_Files.bat

copy "%bundleinfo%\wrapup_cfast_install.bat" "%DISTDIR%\wrapup_cfast_install.bat"
copy "%userprofile%\FIRE-LOCAL\repo_exes\Shortcut.exe"                 "%DISTDIR%\Ssethortcut.exe"
copy "%userprofile%\FIRE-LOCAL\repo_exes\set_path.exe"                 "%DISTDIR%\set_path.exe"

cd %DISTDIR%
wzzip -a -r -P ..\%installerbase%.zip * ..\SMV6 > Nul

:: create an installation file from the zipped bundle directory

echo.
echo ***Creating installer
echo.

cd %DISTDIR%\..
echo Setup is about to install CFAST 7  > %bundleinfo%\message.txt
echo Press Setup to begin installation. > %bundleinfo%\main.txt
if exist %installerbase%.exe erase %installerbase%.exe
wzipse32 %installerbase%.zip -runasadmin -a %bundleinfo%\about.txt -st"cfast 7 Setup" -d "c:\Program Files\firemodels\%distname%" -c wrapup_cfast_install.bat

echo copying %installerbase%.exe to %git_root%\CFAST\uploads\cftest.exe"
copy %installerbase%.exe %git_root%\CFAST\uploads\cftest.exe"


echo.
echo ***cfast bundle built
echo.

cd %CURDIR%
pause



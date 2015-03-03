@echo off
Title Bundle cfast for Windows


:: installation settings settings

set installerbase=cfast6_installer
set distname=cfast6

:: VVVVVVVVVVVVVVVVV shouldn't need to change anything below VVVVVVVVVVVVVVV


set CURDIR=%CD%
cd ..
set svn_root=%CD%
cd %CURDIR%
set svn_drive=c:

%svn_drive%

set DISTDIR=%svn_root%\scripts\BUNDLEDIR\%installerbase%
set bundleinfo=%svn_root%\scripts\bundleinfo

call Create_Install_Files.bat

copy "%bundleinfo%\wrapup_cfast_install.bat" "%DISTDIR%\wrapup_cfast_install.bat"
copy "%bundleinfo%\shortcut.exe" "%DISTDIR%\shortcut.exe"
copy "%bundleinfo%\set_path.exe" "%DISTDIR%\set_path.exe"

cd %DISTDIR%
wzzip -a -r -P ..\%installerbase%.zip * > Nul

:: create an installation file from the zipped bundle directory

echo.
echo ***Creating installer
echo.

cd %DISTDIR%\..
echo Setup is about to install CFAST 6 > %bundleinfo%\message.txt
echo Press Setup to begin installation. > %bundleinfo%\main.txt
if exist %installerbase%.exe erase %installerbase%.exe
wzipse32 %installerbase%.zip -runasadmin -a %bundleinfo%\about.txt -st"cfast 6 Setup" -d "c:\Program Files\%distname%" -c wrapup_cfast_install.bat

copy %installerbase%.exe "%userprofile%\google drive\cftest.exe"


echo.
echo ***cfast bundle built
echo.

cd %CURDIR%




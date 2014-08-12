:: @echo off
Title Bundle cfast for Windows


:: installation settings settings

set installerbase=cftest6
set cfast_version=6.1.11
set distname=cfast%cfast_version%

:: VVVVVVVVVVVVVVVVV shouldn't need to change anything below VVVVVVVVVVVVVVV


set CURDIR=%CD%
cd ..
set svn_root=%CD%
cd %CURDIR%
set svn_drive=c:

%svn_drive%

set DISTDIR=%svn_root%\scripts\BUNDLEDIR\%installerbase%
set bundleinfo=%svn_root%\scripts\bundleinfo

copy "%bundleinfo%\wrapup_cfast_install.bat" "%DISTDIR%\wrapup_cfast_install.bat"
:: erase or comment out following line once actual wrapup batch file is complete
copy "%bundleinfo%\wrapup_dummy.bat" "%DISTDIR%\wrapup_cfast_install.bat"

cd %DISTDIR%
wzzip -a -r -P ..\%installerbase%.zip * > Nul

:: create an installation file from the zipped bundle directory

echo.
echo ***Creating installer
echo.

cd %DISTDIR%\..
echo Setup is about to install CFAST %cfast_version% > %bundleinfo%\message.txt
echo Press Setup to begin installation. > %bundleinfo%\main.txt
if exist %installerbase%.exe erase %installerbase%.exe
wzipse32 %installerbase%.zip -runasadmin -a %bundleinfo%\about.txt -st"cfast %cfast_version% Setup" -d "c:\Program Files\%distname%" -c wrapup_cfast_install.bat


echo.
echo ***cfast bundle built
echo.

cd %CURDIR%




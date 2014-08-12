:: @echo off
Title Bundle cfast for Windows

set basename=cftest6p1

:: Script to bundle cfast into an installation file

set envfile=%userprofile%\cfast_env.bat
IF EXIST %envfile% GOTO endif_envexist
echo ***Fatal error.  The environment setup file %envfile% does not exist. 
echo Aborting now...
pause>NUL
goto:eof

:endif_envexist

call %envfile%
%svn_drive%

set CURDIR=%CD%
set DISTDIR=%svn_root%\scripts\BUNDLEDIR\%basename%
set bundleinfo=%svn_root%\scripts\bundleinfo

copy "%bundleinfo%\wrapup_cfast_install.bat" "%DISTDIR%\wrapup_cfast_install.bat"
copy "%bundleinfo%\wrapup_dummy.bat" "%DISTDIR%\wrapup_cfast_install.bat"

cd %DISTDIR%
wzzip -a -r -P ..\%basename%.zip * > Nul

:: create an installation file from the zipped bundle directory

echo.
echo ***Creating installer
echo.

cd %DISTDIR%\..
echo Setup is about to install CFAST %cfast_version% > %bundleinfo%\message.txt
echo Press Setup to begin installation. > %bundleinfo%\main.txt
if exist %basename%.exe erase %basename%.exe
wzipse32 %basename%.zip -runasadmin -a %bundleinfo%\about.txt -st"cfast %cfast_version% Setup" -d "c:\Program Files\cfast%cfast_version%" -c wrapup_cfast_install.bat


echo.
echo ***cfast bundle built
echo.

cd %CURDIR%




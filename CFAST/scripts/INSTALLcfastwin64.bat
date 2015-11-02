@echo off

Rem Windows batch file to upload Smokeview test files to
Rem the download site.  This script assume that the Windows
Rem batch file, MAKEtest.bat, has already been run.

set envfile="%userprofile%"\cfast_env.bat
IF EXIST %envfile% GOTO endif_envexist
echo ***Fatal error.  The environment setup file %envfile% does not exist. 
echo Create a file named %envfile% and use SMV/scripts/fds_smv_env_template.bat
echo as an example.
echo.
echo Aborting now...
pause>NUL
goto:eof

:endif_envexist

call %envfile%

%git_drive%
echo cd to bundle location
::cd %git_root%\CFAST\uploads

Rem ----------------------------------------------------------
Rem should not need to edit any lines below

set version=%cfast_version%
set platform=win64
set exe=cfast_%version%_%platform%.exe

echo Running Smokeview installer: %exe%
pause
echo call cfast installer
::call  %exe%


pause

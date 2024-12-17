@echo off
Title Packaging test Smokeview for 64 bit Linux

Rem  Windows batch file to create an achive for a 64 bit Linux test smokeview

Rem setup environment variables (defining where repository resides etc) 

set envfile="%userprofile%"\fds_smv_env.bat
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

cd %git_root%\smv\scripts

set scriptdir=FDS-SMV/SMV/scripts
set uploaddir=FDS-SMV/SMV/for_bundle/uploads
set smvdir=smv_%smv_version%_osx64.sh

echo updating 64 bit smokeview
plink %git_logon% %linux_git_root%/SMV/for_bundle/uploads/%exe% y
pause

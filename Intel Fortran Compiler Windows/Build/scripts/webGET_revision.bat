@echo off

Rem  Windows batch file to build a test Smokeview for Windows 64

Rem setup environment variables (defining where repository resides etc) 

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
cd %git_root%
git describe --long --dirty
pause

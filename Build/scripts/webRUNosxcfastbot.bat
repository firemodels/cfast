@echo off
Title run osx cfastbot

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
set scriptdir=%osx_git_root%/Build/scripts

plink %osx_logon% %scriptdir%/RUNosxcfastbot.sh

echo.
echo cfastbot complete
pause

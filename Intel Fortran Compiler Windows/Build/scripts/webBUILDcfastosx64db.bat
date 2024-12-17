@echo off
Title Building cfast for 64 bit OSX

Rem  Windows batch file to build a release Smokeview for Linux 64.

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

plink %osx_logon% %osx_git_root%/Build/scripts/run_command.sh Build/CFAST/intel_osx_64_db make_cfast.sh 

echo.
echo compilation complete
pause

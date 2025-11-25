@echo off
Title Building cfast for 64 bit Windows

Rem  Windows batch file to build a release Smokeview for Windows 64.

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
echo Using the environment variables:
echo.
echo Using GIT revision %smv_revision% to build a 64 bit Windows Smokeview

%git_drive%

cd %git_root%\Build\CFAST\intel_win_db
erase *.obj *.mod
call make_cfast

echo.
echo compilation complete
pause

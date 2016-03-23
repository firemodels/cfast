@echo off

Rem Batch file used to update FDS source revision number

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

Rem location of batch files used to set up Intel compilation environment

call %envfile%

echo.
echo Updating the Windows repository, %git_root%, to the latest revision
%git_drive%
cd %git_root%
echo Updating the repo:%git_root%
git remote update
git merge origin/master

pause

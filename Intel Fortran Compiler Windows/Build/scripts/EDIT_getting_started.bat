@echo off
Rem setup environment variables (defining where repository resides etc) 

set envfile="%userprofile%"\cfast_env.bat
IF EXIST %envfile% GOTO endif_envexist
echo ***Fatal error.  The environment setup file %envfile% does not exist. 
echo Create a file named %envfile% and define the environment
echo variables: git_root, git_drive, smv_version and cluster_logon . Example:
echo.
echo set git_root=c:\cfast
echo set git_drive=c:
echo set git_logon=username@computername
echo.
echo Aborting now...

pause>NUL
goto:eof

:endif_envexist

call %envfile%

%git_drive%
cd %git_root%\Build\scripts
start notepad getting_started.html
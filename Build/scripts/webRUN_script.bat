@echo off

:: setup environment variables (defining where repository resides etc) 

set envfile="%userprofile%"\cfast_env.bat
IF EXIST %envfile% GOTO endif_envexist
echo ***Fatal error.  The environment setup file %envfile% does not exist. 
echo Create a file named %envfile%
echo.
echo Aborting now...

pause>NUL
goto:eof

:endif_envexist

call %envfile%

%git_drive%

call %git_root%\%1


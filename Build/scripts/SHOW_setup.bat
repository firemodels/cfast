 @echo off

Rem setup environment variables (defining where repository resides etc) 

set envfile="%userprofile%"\cfast_env.bat
IF EXIST %envfile% GOTO endif_envexist
echo ***Fatal error.  The environment setup file %envfile% does not exist. 
echo Create a file named %envfile% and use Build/scripts/cfast_env.bat
echo as an example.
echo.
echo Aborting now...
pause>NUL
goto:eof

:endif_envexist

call %envfile%

echo.
echo *** cfast version and revision numbers

echo.
echo cfast_version=%cfast_version%
echo cfast_revision=%cfast_revision%

echo.
echo Press any key to continue
pause>NUL

echo.
echo *** cfast repository settings

echo.
echo git_root=%git_root%
echo git_drive=%git_drive%
echo linux_git_root=%linux_git_root%
echo osx_git_root=%osx_git_root%

echo.
echo Press any key to continue
pause>NUL

echo.
echo *** Linux User/Host names

echo linux_hostname=%linux_hostname%
echo linux_username=%linux_username%

echo.
echo *** OSX User/Host names

echo osx_hostname=%osx_hostname%
echo osx_username=%osx_username%


echo.
echo Press any key to continue
pause>NUL

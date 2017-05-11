@echo off

:: get a sha1 hash for every .exe file in the same directory containing arg

set arg=%1

set CURDIR=%CD%
where %arg% | head -1 > argpath.txt
set /p I=<argpath.txt
call :get_pathonly "%I%"
cd %pathonly%

echo sha1 hashes for programs in %pathonly%:
for /r %%v in (*.exe) do if exist "%%~nv%%~xv" hashfile "%%~nv%%~xv"

cd %CURDIR%
erase argpath.txt
goto eof

:: -------------------------------------------------------------
  :get_pathonly
:: -------------------------------------------------------------

set pathonly=%~dp1
exit /b 0

:eof
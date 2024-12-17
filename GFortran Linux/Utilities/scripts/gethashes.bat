@echo off

:: get a sha1 hash for every .exe file in the same directory containing arg

set argdir=%1

if not exist %argdir% (
  echo "***error: the directory %argdir% does not exist"
  exit /b
)

set CURDIR=%CD%



cd %argdir%

for /r %%v in (*.exe) do if exist "%%~nv%%~xv" hashfile "%%~nv%%~xv"

cd %CURDIR%

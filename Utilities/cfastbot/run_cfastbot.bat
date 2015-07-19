@echo off
:: usage: 
::  run_cfastbot -cfastrepo name -fdsrepo name echo -email address -nomatlab
::  (all command arguements are optional)

call getopts %*
if %stopscript% == 1 (
  exit /b
)

set gitrepo=%userprofile%\%cfastrepo%
set curdir=%CD%
set running=%curdir%\bot.running

cd %gitrepo%
git pull
copy Utilities\cfastbot\cfastbot_win.bat %curdir%
cd %curdir%

if not exist %running% (
  echo 1 > %running%
  call cfastbot_win.bat %cfastrepo% %fdsrepo% %usematlab% %emailto%
  erase %running%
  cd %curdir%
) else (
  echo cfastbot is currently running.
  echo If this is not the case, erase the file %running%
)

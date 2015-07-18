@echo off
:: usage: 
::  run_cfastbot -email email_address -repo repo_name -nomatlab

set repobase=cfastgitclean
set emailto=
set usematlab=1
set stopscript=0

call getopts %*
if %stopscript% == 1 (
  exit /b
)

set gitrepo=%userprofile%\%repobase%
set curdir=%CD%
set running=%curdir%\bot.running

cd %gitrepo%
git pull
copy Utilities\cfastbot\cfastbot_win.bat %curdir%
cd %curdir%

if not exist %running% (
  echo 1 > %running%
  call cfastbot_win.bat %usematlab% %emailto%
  erase %running%
  cd %curdir%
) else (
  echo cfastbot is currently running.
  echo If this is not the case, erase the file %running%
)

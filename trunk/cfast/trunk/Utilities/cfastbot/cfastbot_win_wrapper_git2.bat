@echo off
set emailto=%1

set gitrepo=%userprofile%\cfastgitclean
set curdir=%CD%
set running=%curdir%\bot.running

cd %gitrepo%
git pull
copy Utilities\cfastbot\cfastbot_win_git.bat %curdir%
cd %curdir%

if not exist %running% (
  echo 1 > %running%
  call cfastbot_win_git.bat 0  %emailto%
  erase %running%
  cd %curdir%
) else (
  echo A bot is already running.
  echo Erase the file %running% if this is not the case
)

@echo off
set emailto=%1
set usematlab=1

set gitrepo=%userprofile%\cfastgitclean
set curdir=%CD%
set running=%curdir%\bot.running

cd %gitrepo%
git pull
copy Utilities\cfastbot\cfastbot_win_git.bat %curdir%
cd %curdir%

if not exist %running% (
  echo 1 > %running%
  call cfastbot_win_git.bat %usematlab% %emailto%
  erase %running%
  cd %curdir%
) else (
  echo cfastbot is currently running.
  echo If this is not the case, erase the file %running%
)

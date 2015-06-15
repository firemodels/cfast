@echo off
set arg=%1

set curdir=%CD%
set running=%curdir%\bot.running
if not exist %running% (
  echo 1 > %running%
  call cfastbot_win_git.bat  %arg%
  erase %running%
  cd %curdir%
) else (
  echo A bot is already running.
  echo Erase the file %running% if this is not the case
)

@echo off
set emailto=%1
set usematlab=0

set curdir=%CD%
set running=%curdir%\bot.running
if not exist %running% (
  svn update
  echo 1 > %running%
  call cfastbot_win.bat %usematlab% %emailto%
  erase %running%
  cd %curdir%
) else (
  echo cfastbot is currently running.
  echo If this is not the case, erase the file %running% .
)

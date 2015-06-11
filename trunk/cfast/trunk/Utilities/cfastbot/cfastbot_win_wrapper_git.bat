@echo off
set arg=%1

:: $Date: 2015-01-22 22:30:50 -0500 (Thu, 22 Jan 2015) $ 
:: $Revision: 21509 $
:: $Author: gforney $

set curdir=%CD%
set running=%curdir%\bot.running
if not exist %running% (
  svn update
  echo 1 > %running%
  call cfastbot_win_git.bat  %arg%
  erase %running%
  cd %curdir%
) else (
  echo A bot is already running.
  echo Erase the file %running% if this is not the case
)

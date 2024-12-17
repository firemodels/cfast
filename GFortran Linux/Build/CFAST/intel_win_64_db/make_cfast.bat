@echo off
set arg1=%1
set arg2=%2

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\..\scripts\setup_intel_compilers.bat intel64
:skip1

set version=Test Version     :
if "x%arg2%" == "xrelease" (
  set version=Release Version  :
)

Title Building cfast for 64 bit Windows

make SHELL="%ComSpec%" VERSION="%version%" -f ..\makefile intel_win_64_db
if x%arg1% == xbot goto skip2
pause
:skip2

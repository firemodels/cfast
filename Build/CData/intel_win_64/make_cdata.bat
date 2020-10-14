@echo off
set arg1=%1
set arg2=%2
set md5hash=..\..\..\Utilities\scripts\md5hash.bat

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\..\scripts\setup_intel_compilers.bat intel64
:skip1

set version=Test Version     :
if "x%arg2%" == "xrelease" (
  set version=Release Version  :
)

Title Building cdata for 64 bit Windows

make SHELL="%ComSpec%" VERSION="%version%" -f ..\makefile intel_win_64
%md5hash% cdata7_win_64.exe
if x%arg1% == xbot goto skip2
pause
:skip2

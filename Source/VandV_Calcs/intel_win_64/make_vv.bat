@echo off
set arg1=%1

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\..\CFAST\scripts\setup_intel_compilers.bat intel64
:skip1

Title VV app for 64 bit Windows

make VPATH=".." -f ..\makefile intel_win_64
if x%arg1% == xbot goto skip2
pause
:skip2

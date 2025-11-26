@echo off
set arg1=%1

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\..\..\Source\CFAST\scripts\setup_intel_compilers.bat intel64
:skip1

Title Building background for 64 bit Windows

erase *.obj *.mod
make SHELL="%ComSpec%" -f ..\Makefile intel_win
if x%arg1% == xbot goto skip2
pause
:skip2

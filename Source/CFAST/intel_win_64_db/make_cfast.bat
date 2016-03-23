@echo off
set arg1=%1

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\scripts\setup_intel_compilers.bat intel64
:skip1

Title Building debug cfast for 64 bit Windows

make VPATH="../Source:../Include" SHELL="%ComSpec%" INCLUDE="../Include" -f ..\makefile intel_win_64_db
if x%arg1% == xbot goto skip2
pause
:skip2

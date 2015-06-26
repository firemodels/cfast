@echo off

:: setup compiler environment
call ..\..\CFAST\scripts\setup_intel_compilers.bat intel64

Title Building cfast for 64 bit Windows

make VPATH=".." -f ..\makefile intel_win_64
pause

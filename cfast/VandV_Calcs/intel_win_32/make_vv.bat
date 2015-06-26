@echo off

:: setup compiler environment
call ..\..\CFAST\scripts\setup_intel_compilers.bat ia32

Title Building VandV_Calcs for 32bit Windows

make VPATH=".." -f ..\makefile intel_win_32
pause

@echo off
:: setup compiler environment
call ..\..\..\Source\CFAST\scripts\setup_intel_compilers.bat intel64

Title Building make_time for 64 bit Windows

erase *.obj *.mod
make -f ..\Makefile intel_win
pause


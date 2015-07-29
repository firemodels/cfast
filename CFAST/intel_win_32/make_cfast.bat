@echo off

:: setup compiler environment
call ..\scripts\setup_intel_compilers.bat ia32

Title Building cfast for 32 bit Windows

make VPATH="../Source:../Include" SHELL="%ComSpec%" INCLUDE="../Include" -f ..\makefile intel_win_32
pause


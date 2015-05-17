@echo off

:: setup compiler environment
call ..\scripts\setup_intel_compilers.bat intel64

Title Building cfast for 64 bit Windows

call ..\scripts\expand_file ..\Source srev.f90
make VPATH="../Source:../Include" INCLUDE="../Include" -f ..\makefile intel_win_64
call ..\scripts\contract_file ..\Source\srev.f90
pause


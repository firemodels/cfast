@echo off

:: setup compiler environment
call ..\scripts\setup_intel_compilers.bat intel64

Title Building cfast for 64 bit Windows

call ..\scripts\get_revision ../Source
call ..\scripts\make_frevision ../Source srev.f90

make VPATH="../Source:../Include" INCLUDE="../Include" -f ..\makefile intel_win_64
pause


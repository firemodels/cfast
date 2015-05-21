@echo off

:: setup compiler environment
call ..\scripts\setup_intel_compilers.bat intel64
set KWDIR=..\..\Utilities\keyword
set SDIR=..\Source

Title Building cfast for 64 bit Windows

call %KWDIR%/expand_file %KWDIR% %SDIR% %SDIR%/srev.f90
make VPATH="../Source:../Include" INCLUDE="../Include" -f ..\makefile intel_win_64
call %KWDIR%/contract_file %KWDIR% %SDIR%/srev.f90
pause

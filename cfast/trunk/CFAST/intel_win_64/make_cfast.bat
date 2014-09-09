@echo off
set intelbin="%IFORT_COMPILER15%\bin"

IF "%SETUP_IFORT_COMPILER_64%"=="1" GOTO envexist

set SETUP_IFORT_COMPILER_64=1

echo Setting up compiler environment
call %intelbin%\ifortvars intel64

:envexist
make VPATH="../Source:../Include" INCLUDE="../Include" -f ..\makefile intel_win_64
pause


@echo off

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\..\Build\scripts\setup_intel_compilers.bat intel64
:skip1


icl -o sh2bat sh2bat.c
pause

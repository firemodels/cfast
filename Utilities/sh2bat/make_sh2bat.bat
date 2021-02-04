@echo off
set arg1=%1

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\..\Build\scripts\setup_intel_compilers.bat intel64
:skip1


icl -o sh2bat sh2bat.c

if x%arg1% == xnopause goto skip2
pause
:skip2

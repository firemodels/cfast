@echo off
:: setup compiler environment
call ..\..\..\Source\CFAST\scripts\setup_intel_compilers.bat intel64

set SMV_TESTFLAG=
set SMV_TESTSTRING=

if "%1" NEQ "-t" goto endif
  set SMV_TESTFLAG=-D pp_BETA
  set SMV_TESTSTRING=test_
:endif

erase *.obj
icl -o runbatch_win_64.exe ..\main.c



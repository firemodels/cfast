@echo off

set emailto=
set usematlab=1
set stopscript=0
set cfastrepo=cfastgitclean
set fdsrepo=FDS-SMVgitclean
set stopscript=0

set /a nargs=0
for %%a in (%*) do set /a nargs+=1

if %nargs% == 0 (
  call :usage
  set stopscript=1
  exit /b
)

:GETOPTS
 if /I "%1" EQU "-cfastrepo" (
   set cfastrepo=%2
   shift
 )
 if /I "%1" EQU "-fdsrepo" (
   set fdsrepo=%2
   shift
 )
 if /I "%1" EQU "-email" (
   set emailto=%2
   shift
 )
 if /I "%1" EQU "-nomatlab" (
   set usematlab=0
 )
 if /I "%1" EQU "-runbot" (
   set stopscript=0
 )
 shift
if not (%1)==() goto GETOPTS
exit /b

:usage  run_cfastbot [options]
echo 
echo -cfastrepo name - specify the cfast repo name (default: cfastgitclean) 
echo -fdsrepo name   - specify the FDS repo name (default: FDS-SMVgitclean) 
echo -email address  - override "to" email addresses specified in repo 
echo -nomatlab       - do not use matlab
echo -runbot         - run cfastbot


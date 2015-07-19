@echo off

set emailto=
set usematlab=1
set stopscript=0
set cfastrepo=cfastgitclean
set fdsrepo=FDS-SMVgitclean
set stopscript=0

:GETOPTS
 if /I "%1" EQU "-h" (
   call :usage
   set stopscript=1
 )
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
 shift
if not (%1)==() goto GETOPTS
exit /b

:usage  run_cfastbot [options]
echo 
echo -h              - display this message
echo -cfastrepo name - specify the cfast repo name (default: cfastgitclean) 
echo -fdsrepo name   - specify the FDS repo name (default: FDS-SMVgitclean) 
echo -email address  - override "to" email addresses specified in repo 
echo -nomatlab       - do not use matlab


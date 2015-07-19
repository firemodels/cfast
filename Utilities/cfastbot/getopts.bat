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
 if /I "%1" EQU "-help" (
   call :usage
   set stopscript=1
 )
 if /I "%1" EQU "-cfastrepo" (
   set cfastrepo=%2
   shift
 )
 if /I "%1" EQU "-c" (
   set cfastrepo=%2
   shift
 )
 if /I "%1" EQU "-fdsrepo" (
   set fdsrepo=%2
   shift
 )
 if /I "%1" EQU "-f" (
   set fdsrepo=%2
   shift
 )
 if /I "%1" EQU "-email" (
   set emailto=%2
   shift
 )
 if /I "%1" EQU "-e" (
   set emailto=%2
   shift
 )
 if /I "%1" EQU "-nomatlab" (
   set usematlab=0
 )
 if /I "%1" EQU "-n" (
   set usematlab=0
 )
 shift
if not (%1)==() goto GETOPTS
exit /b

:usage  run_cfastbot [options]
echo 
echo -help|-h           - display this message
echo -cfastrepo|-c name - specify the cfast repo name (default: cfastgitclean) 
echo -fdsrepo|-f name   - specify the FDS repo name (default: FDS-SMVgitclean) 
echo -email|-e address  - override "to" email addresses specified in repo 
echo -nomatlab|-n       - do not use matlab


@echo off

set stopscript=0
:GETOPTS
 if /I "%1" EQU "-h" (
   call :usage
   set stopscript=1
 )
 if /I "%1" EQU "-repo" (
   set repobase=%2
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
echo -repo repo_name - specify the git repo name (defaul t: cfastgitclean) 
echo -email address  - specify the email address to send cfastbot results 
echo -nomatlab       - do not use matlab


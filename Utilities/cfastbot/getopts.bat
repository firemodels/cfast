@echo off
:: for now only support -r, -e and -m

:GETOPTS
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


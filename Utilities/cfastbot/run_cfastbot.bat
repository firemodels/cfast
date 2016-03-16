@echo off
:: usage: 
::  run_cfastbot -cfastrepo name -fdsrepo name -email address -nomatlab -updateclean -update -clean 
::  (all command arguements are optional)

set usematlab=1
set update=0
set clean=0
set stopscript=0
set installed=0
set force=0
set skip_cases=0
set official=0

set RUNDIR=%CD%

set cfastrepo=%userprofile%\cfastgitclean

if x%CFASTGIT% == x goto skip_cfastgit
  if EXIST %CFASTGIT% (
    set cfastrepo=%CFASTGIT%
  )
:skip_cfastgit

if NOT exist .cfast_git goto skip_if1
  set cfastrepo=%RUNDIR%\..\..
  call :normalise %cfastrepo%
  set cfastrepo=%temparg%
:skip_if1

set fdsrepo=none
if exist %userprofile%\FDS-SMVgitclean (
  set fdsrepo=%userprofile%\FDS-SMVgitclean
)
if x%FDSGIT% == x goto skip_fdsgit
  if EXIST %FDSGIT% (
    set fdsrepo=%FDSGIT%
  )
:skip_fdsgit

set emailto=
if not x%EMAILGIT% == x (
  set emailto=%EMAILGIT%
)

:: parse command line arguments

call :normalise %cfastrepo% 
set cfastrepo=%temparg%

if %fdsrepo% == none goto skip_fdsrepo
  call :normalise %fdsrepo%
  set fdsrepo=%temparg%
)
:skip_fdsrepo

set stopscript=0
call :getopts %*
if %stopscript% == 1 (
  exit /b
)

:: normalize directory paths

call :normalise %CD% curdir
set curdir=%temparg%

call :normalise %cfastrepo%\Utilities\cfastbot
set cfastbotdir=%temparg%

call :normalise %cfastrepo% 
set cfastrepo=%temparg%

if %fdsrepo% == none goto skip_fdsrepo2
  call :normalise %fdsrepo%
  set fdsrepo=%temparg%
:skip_fdsrepo2

set running=%curdir%\bot.running

if %force% == 1 erase %running% 1> Nul 2>&1
if exist %running% goto skip_running

:: get latest cfastbot

if %update% == 0 goto no_update
  echo getting latest cfastbot
  cd %cfastrepo%
  git fetch origin
  git pull 1> Nul 2>&1
  if not %cfastbotdir% == %curdir% (
    copy %cfastbotdir%\cfastbot.bat %curdir%
  )
  cd %curdir%
:no_update

:: run cfastbot

  echo 1 > %running%
  call cfastbot.bat %cfastrepo% %fdsrepo% %usematlab% %clean% %update% %installed% %skip_cases% %official% %emailto%
  erase %running%
  goto end_running
:skip_running
  echo cfastbot is currently running. If this is not the case,
  echo run run_cfastbot.bat again using the -force option
:end_running

goto eof

:getopts
 if (%1)==() exit /b
 set valid=0
 set arg=%1
 if /I "%1" EQU "-help" (
   call :usage
   set stopscript=1
   exit /b
 )
 if /I "%1" EQU "-cfastrepo" (
   set cfastrepo=%2
   set valid=1
   shift
 )
 if /I "%1" EQU "-fdsrepo" (
   set fdsrepo=%2
   set valid=1
   shift
 )
 if /I "%1" EQU "-email" (
   set emailto=%2
   set valid=1
   shift
 )
 if /I "%1" EQU "-force" (
   set force=1
   set valid=1
 )
 if /I "%1" EQU "-nomatlab" (
   set valid=1
   set usematlab=0
 )
 if /I "%1" EQU "-official" (
   set official=1
   set valid=1
 )
 if /I "%1" EQU "-installed" (
   set valid=1
   set installed=1
 )
 if /I "%1" EQU "-bot" (
   set valid=1
   set clean=1
   set update=1
 )
 if /I "%1" EQU "-skip_cases" (
   set valid=1
   set skip_cases=1
 )
 if /I "%1" EQU "-update" (
   set valid=1
   set update=1
 )
 if /I "%1" EQU "-clean" (
   set valid=1
   set clean=1
 )
 shift
 if %valid% == 0 (
   echo.
   echo ***Error: the input argument %arg% is invalid
   echo.
   echo Usage:
   call :usage
   set stopscript=1
   exit /b
 )
if not (%1)==() goto getopts
exit /b

:usage  
echo run_cfastbot [options]
echo. 
echo -help           - display this message
echo -cfastrepo name - specify the cfast repository
echo       (default: %cfastrepo%) 
echo -fdsrepo name   - specify the FDS-SMV repository
echo       (default: %fdsrepo%) 
echo -email address  - override "to" email addresses specified in repo 
if "%emailto%" NEQ "" (
echo       (default: %emailto%^)
)
echo -bot            - update and clean repository
echo -skip_cases     - skip over stage that runs cases
echo -force          - force cfastbot run
echo -installed      - use installed smokeview
echo -clean          - clean repository
echo -update         - update repository
echo -nomatlab       - do not use matlab
echo -official       - build official version of cfast
exit /b

:normalise
set temparg=%~f1
exit /b

:eof


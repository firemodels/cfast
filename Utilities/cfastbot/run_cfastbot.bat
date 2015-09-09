@echo off
:: usage: 
::  run_cfastbot -cfastrepo name -fdsrepo name echo -email address -nomatlab
::  (all command arguements are optional)

set usematlab=1
set stopscript=0

set cfastrepo=%userprofile%\cfastgitclean
if "%CFASTGIT%" NEQ "" (
  if EXIST %CFASTGIT% (
    set cfastrepo=%CFASTGIT%
  )
)

set fdsrepo=%userprofile%\FDS-SMVgitclean
if "%FDSGIT%" NEQ "" (
  if EXIST %FDSGIT% (
    set fdsrepo=%FDSGIT%
  )
)

set emailto=
if "%EMAILGIT%" NEQ "" (
  set emailto=%EMAILGIT%
)

set stopscript=0


call :getopts %*
if %stopscript% == 1 (
  exit /b
)

call :normalise %CD% curdir
call :normalise %cfastrepo%\Utilities\cfastbot cfastbotdir
call :normalise %cfastrepo% cfastrepo
call :normalise %fdsrepo% fdsrepo
set running=%curdir%\bot.running

if not exist %running% (
  if %cfastbotdir% NEQ %curdir% (
    cd %cfastrepo%
    git fetch origin
    git pull
    copy Utilities\cfastbot\cfastbot_win.bat %curdir%
    cd %curdir%
  )
  echo 1 > %running%
  echo cfastbot_win.bat %cfastrepo% %fdsrepo% %usematlab% %emailto%
  call cfastbot_win.bat %cfastrepo% %fdsrepo% %usematlab% %emailto%
  erase %running%
) else (
  echo cfastbot is currently running.
  echo If this is not the case, erase the file %running%
)

goto eof

:getopts
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
 if /I "%1" EQU "-nomatlab" (
   set valid=1
   set usematlab=0
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
echo -nomatlab       - do not use matlab
exit /b

:normalise
SET "%2=%~f1"
exit /b

:eof


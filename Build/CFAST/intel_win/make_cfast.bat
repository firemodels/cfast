@echo off
setlocal
set clean_cfast=false
set arg1=
set arg2=

:parse_args
if "%~1"=="" goto args_done
if /I "%~1"=="--clean-cfast" (
  set clean_cfast=true
  shift
  goto parse_args
)
if /I "%~1"=="--help" goto usage
if /I "%~1"=="-h" goto usage
if /I "%~1"=="bot" (
  set arg1=bot
  shift
  goto parse_args
)
if /I "%~1"=="release" (
  set arg2=release
  shift
  goto parse_args
)
echo Unknown option: %~1 1>&2
exit /b 1

:usage
echo Usage: make_cfast.bat [bot] [release] [--clean-cfast]
echo   --clean-cfast  Remove object and module files before building; skip pause.
exit /b 0

:args_done
set md5hash=..\..\..\Utilities\scripts\md5hash.bat

:: setup compiler environment
if x%arg1% == xbot goto skip1
call ..\..\scripts\setup_intel_compilers.bat intel64
:skip1

set version=Test Version     :
if "x%arg2%" == "xrelease" (
  set version=Release Version  :
)

Title Building cfast for 64 bit Windows

if "%clean_cfast%"=="true" (
  echo Option --clean-cfast is set.
  make SHELL="%ComSpec%" -f ..\makefile winclean
  if errorlevel 1 exit /b 1
)

make SHELL="%ComSpec%" VERSION="%version%" -f ..\makefile intel_win
if errorlevel 1 exit /b 1
call %md5hash% cfast8_win.exe
if errorlevel 1 exit /b 1
if x%arg1% == xbot goto skip2
if "%clean_cfast%"=="false" pause
:skip2
exit /b 0

@echo off
setlocal
set clean_cfast=false

:parse_args
if "%~1"=="" goto args_done
if /I "%~1"=="--clean-cfast" (
  set clean_cfast=true
  shift
  goto parse_args
)
if /I "%~1"=="--help" goto usage
if /I "%~1"=="-h" goto usage
echo Unknown option: %~1 1>&2
exit /b 1

:usage
echo Usage: make_cfast.bat [--clean-cfast]
echo   --clean-cfast  Remove object and module files before building; skip pause.
exit /b 0

:args_done

Title Building cfast for 64 bit Windows

if "%clean_cfast%"=="true" (
  echo Option --clean-cfast is set.
  make SHELL="%ComSpec%" -f ..\makefile winclean
  if errorlevel 1 exit /b 1
)

make -f ..\makefile gnu_win
if errorlevel 1 exit /b 1
if "%clean_cfast%"=="false" pause
exit /b 0

@echo off

IF  X%SETVARS_COMPLETED% == X1 GOTO intel_envexist

  set "ONEAPIDIR=C:\Program Files (x86)\Intel\oneAPI"
  IF DEFINED ONEAPI_ROOT set "ONEAPIDIR=%ONEAPI_ROOT%"
  IF NOT EXIST "%ONEAPIDIR%\setvars.bat" goto intel_notexist

  echo Defining Intel compiler environment
  call "%ONEAPIDIR%\setvars" intel64>Nul

  IF  X%SETVARS_COMPLETED% == X1 GOTO intel_envexist

:intel_notexist
  echo ***error: Intel oneAPI installation not found at %ONEAPIDIR% .
  echo           Compiler environment is not setup.
  echo           set the environment variable ONEAPI_ROOT to the oneAPI location if it is installed.
  goto :eof

:intel_envexist
:eof

@echo off

IF  X%SETVARS_COMPLETED% == X1 GOTO intel_envexist

  IF NOT DEFINED ONEAPI_ROOT goto intel_notexist

  echo Defining Intel compiler environment
  call "%ONEAPI_ROOT%\setvars" intel64>Nul

  IF  X%SETVARS_COMPLETED% == X1 GOTO intel_envexist

:intel_notexist
  echo ***error: Intel compiler environment is not setup
  goto :eof

:intel_envexist
:eof

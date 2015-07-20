@echo off
set CURDIR=%CD%
set repo=%1
set repodir=%userprofile%\%repo%

if EXIST %repodir% (
  cd %userprofile%\%repo%
  echo Updating repo
  git fetch origin
  git pull
  cd %CURDIR%
  echo copying files
  copy %repodir%\Utilities\cfastbot\*.bat
) else (
  echo ***Error: The directory %repodir% does not exist. Copy aborted.
)

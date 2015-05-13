@echo off
set dir=%1
set file=%2

set CURDIR=%CD%

if NOT exist %dir% (
  exit /b 1
)

set fullfile=%dir%\%file%
set fullfilebak=%dir%\%file%bak
copy %fullfilebak% %fullfile%



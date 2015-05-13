@echo off
set dir=%1
set file=%2

if NOT exist %dir% (
  exit /b 1
)

set fullfile=%dir%\%file%
set fullfilebak=%dir%\%file%bak
if exist %fullfilebak% (
  copy %fullfilebak% %fullfile%
)



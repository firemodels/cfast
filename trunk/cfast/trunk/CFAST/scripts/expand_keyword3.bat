@echo off
set bindir=%1
set keyword=%2
set dir=%3
set file=%4

set fullfile=%dir%\%file%

if NOT exist %fullfile% (
  exit /b 1
)

call %bindir%\get_revision %dir%

call %bindir%\expand_keyword %keyword% %revision% %fullfile%

@echo off
set dir=%1
set file=%2

set fullfile=%dir%\%file%

if NOT exist %fullfile% (
  exit /b 1
)

call ..\scripts\get_revision %dir%

call ..\scripts\expand_keyword Revision: %svn_revision% %fullfile%
call ..\scripts\expand_keyword RevisionDate: %svn_date% %fullfile%
call ..\scripts\expand_keyword CompileDate: %datetime% %fullfile%

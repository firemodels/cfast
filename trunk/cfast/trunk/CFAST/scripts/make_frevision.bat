@echo off
set dir=%1
set file=%2

set fullfile=%dir%\%file%

if NOT exist %fullfile% (
  exit /b 1
)

call ..\scripts\get_revision %dir%

call ..\scripts\expand_keyword Revision: %revision% %fullfile%
call ..\scripts\expand_keyword2 RevisionDate: %revision_date% %revision_time% %fullfile%
call ..\scripts\expand_keyword2 CompileDate: %build_date% %build_time% %fullfile%

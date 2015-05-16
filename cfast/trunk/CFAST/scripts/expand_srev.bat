@echo off

:: expand keywords Revision, RevisionDate and CompileDate in file

set bindir=%~p0
set dir=%1
set file=%2

set fullfile=%dir%\%file%

if NOT exist %fullfile% (
  exit /b 1
)

call "%bindir%\get_repo_properties" %dir%

call "%bindir%\expand_keyword" Revision %revision% %fullfile%
call "%bindir%\expand_keyword" RevisionDate "%revision_date% %revision_time%" %fullfile%
call "%bindir%\expand_keyword" CompileDate "%build_date% %build_time%" %fullfile%

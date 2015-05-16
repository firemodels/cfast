@echo off

:: get svn or git revision of directory dir 
:: then expand keyword Revision in file using this property

set bindir=%~p0
set dir=%1
set file=%2

set fullfile=%dir%\%file%

if NOT exist %fullfile% (
  exit /b 1
)

call "%bindir%\get_repo_properties" %dir%
call "%bindir%\expand_keyword" Revision %revision% %fullfile%

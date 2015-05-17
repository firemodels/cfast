@echo off

:: get the svn (or git) revision of directory dir 
:: then expand the keyword Revision in file using this property

if "%1" == "" (
  echo usage: expand_revision dir file
  echo        in dir\file, convert all occurrences of $Revision: .... $ to
  echo        $Revision: rev_number $
  echo        where rev_number is the revision number of the directory dir
  goto eof
)

set bindir=%~p0
set dir=%1
set file=%2

set fullfile=%dir%\%file%

if NOT exist %fullfile% (
  exit /b 1
)

call "%bindir%\get_repo_properties" %dir%
call "%bindir%\expand_keyword" Revision %revision% %fullfile%

:eof
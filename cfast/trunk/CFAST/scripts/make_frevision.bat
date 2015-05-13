@echo off
set dir=%1
set file=%2

set CURDIR=%CD%

if NOT exist %dir% (
  exit /b 1
)

set fullfile=%dir%\%file%
set fullfilebak=%dir%\%file%bak
copy %fullfile% %fullfilebak%


echo     subroutine get_revision (revision)>%fullfile%
echo     implicit none>>%fullfile%
echo     character(len=256), intent(out) :: revision>>%fullfile%
echo     revision = trim("%svn_revision%")>>%fullfile%
echo     return>>%fullfile%
echo     end subroutine get_revision>>%fullfile%




@echo off
set dir=%1
set file=%2

if NOT exist %dir% (
  exit /b 1
)

set fullfile=%dir%\%file%
set fullfilebak=%dir%\%file%bak
copy %fullfile% %fullfilebak%


echo     subroutine get_revision (revision,date)>%fullfile%
echo     implicit none>>%fullfile%
echo     character(len=256), intent(out) :: revision, date>>%fullfile%
echo     revision = trim(adjustl("%svn_revision%"))>>%fullfile%
echo     date = trim(adjustl("%svn_date%"))>>%fullfile%
echo     return>>%fullfile%
echo     end subroutine get_revision>>%fullfile%




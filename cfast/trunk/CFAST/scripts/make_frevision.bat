@echo off
set dir=%1
set file=%2

if NOT exist %dir% (
  exit /b 1
)

set fullfile=%dir%\%file%
set fullfilebak=%dir%\%file%bak
copy %fullfile% %fullfilebak%


echo     subroutine get_info (revision,revision_date,compile_date)>%fullfile%
echo     implicit none>>%fullfile%
echo     character(len=256), intent(out) :: revision, revision_date, compile_date>>%fullfile%
echo     revision = trim(adjustl("%svn_revision%"))>>%fullfile%
echo     revision_date = trim(adjustl("%svn_date%"))>>%fullfile%
echo     compile_date = trim(adjustl("%date% %time%"))>>%fullfile%
echo     return>>%fullfile%
echo     end subroutine get_info>>%fullfile%




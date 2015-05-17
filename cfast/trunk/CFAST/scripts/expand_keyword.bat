@echo off

:: expand keyword found in file using newvalue
::   (note that the ~ in %~2 removes surrounding quotes (allowing imbedded blanks in newvalue)

set keyword=%1
set newvalue=%~2
set file=%3

if NOT exist %file% (
  exit 1 /b
)
sed -e "s/$%keyword%:.*\$/$%keyword%: %newvalue% $/g" %file% | sed "s/$/\r/" > %temp%\temp.txt
copy %temp%\temp.txt %file% 1> Nul 2>&1

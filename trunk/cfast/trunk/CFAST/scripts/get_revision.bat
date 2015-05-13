@echo off
set svn_dir=%1

set svn_revision=unknown
set svn_date=unknown
set datetime=unknown

set temp1=%temp%\temp.txt
set temp1c=%temp%\tempc.txt

set CURDIR=%CD%

:: looking for svn

svn 1> %temp1% 2>&1
type %temp1% | find /i /c "not recognized" > %temp1c%
if %temp1c% == 1 (
  exit /b 1
)

:: looking for gawk

gawk 1> %temp1% 2>&1
type %temp1% | find /i /c "not recognized" > %temp1c%
if %temp1c% == 1 (
  exit /b 1
)

:: check to see if %svn_dir% exists

if NOT exist %svn_dir% (
  exit /b 1
)

cd %svn_dir%

:: is this a valid svn repository

svn 1> %temp1% 2>&1
type %temp1% | find /i /c "not a working copy" > %temp1c%
if %temp1c% == 1 (
  set svn_revision=invalid
  cd %CURDIR%
  exit /b 1
)

:: get svn revision number

svn info 2>&1 | find /i "Last Changed Rev:" | gawk -F" " "{print $4}" > %temp1%
set /p svn_revision=<%temp1%

:: get svn date

svn info 2>&1 | find /i "Last Changed Date:" | gawk -F" " "{$1=\"\";$2=\"\";$3=\"\";print $0}" > %temp1%
set /p svn_date=<%temp1%

:: get current date time

echo %time% 2>&1 | gawk -F":" "{print $1\":\"$2}" > %temp1%
set /p hmtime=<%temp1%
set datetime=%date% %hmtime%

cd %CURDIR%

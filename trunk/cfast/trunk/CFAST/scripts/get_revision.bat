@echo off
set svn_dir=%1

set svn_revision=unknown

set temp1=temp.txt
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
svn info 2>&1 | find /i "Last Changed Rev:" | gawk -F" " "{print $4}" > %temp1%
set /p svn_revision=<%temp1%

cd %CURDIR%

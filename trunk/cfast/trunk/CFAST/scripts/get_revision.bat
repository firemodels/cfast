@echo off
set svn_dir=%1

set revision=unknown

set revision_date=unknown
set revision_time=unknown

set build_date=unknown
set build_time=unknown

set havesvn=1
set havegit=1

set validsvn=0
set validgit=0

set temp1=%temp%\temp.txt
set temp1c=%temp%\tempc.txt

set CURDIR=%CD%

:: looking for svn

svn 1> %temp1% 2>&1
type %temp1% | find /i /c "not recognized" > %temp1c%
set flag=
set /p flag=<%temp1c%
if %flag% == 1 (
  set havesvn=0
)

:: looking for git

git 1> %temp1% 2>&1
type %temp1% | find /i /c "not recognized" > %temp1c%
set flag=
set /p flag=<%temp1c%
if %flag% == 1 (
  set havegit=0
  if %havesvn% == 0 (
    echo *** warning: both svn and git were not found
    exit /b 1
  )
)

:: looking for head (only used with git)

if %havegit% == 1 (
  head -h 1> %temp1% 2>&1
  type %temp1% | find /i /c "not recognized" > %temp1c%
  set flag=
  set /p flag=<%temp1c%
  if %flag% == 1 (
    echo *** warning: head was not found.
    exit /b 1
  )
)

:: looking for tail (only used with git)

if %havegit% == 1 (
  tail -h 1> %temp1% 2>&1
  type %temp1% | find /i /c "not recognized" > %temp1c%
  set flag=
  set /p flag=<%temp1c%
  if %flag% == 1 (
    echo *** warning: tail was not found.
    exit /b 1
  )
)

:: looking for gawk

gawk 1> %temp1% 2>&1
type %temp1% | find /i /c "not recognized" > %temp1c%
set flag=
set /p flag=<%temp1c%
if %flag% == 1 (
  echo *** warning: gawk was not found.
  exit /b 1
)

:: check to see if %svn_dir% exists

if NOT exist %svn_dir% (
  echo *** warning: The directory %svn_dir% does not exist.
  exit /b 1
)

cd %svn_dir%

:: is this a valid svn repository

if %havesvn% == 1 (
  set validsvn=1
  svn info 1> %temp1% 2>&1
  type %temp1% | grep "not a working copy" | wc -l > %temp1c%
  set glag=
  set /p glag=<%temp1c%
  if %glag% == 1 (
    set svn_revision=invalid
    cd %CURDIR%
    set validsvn=0
    if %havegit% == 0 (
      echo "*** warning: %svn_dir% is not a valid svn repository"
      exit /b 1
    )
  )
)

:: is this a valid git repository

if %havegit% == 1 (
  if %validsvn% == 0 (
    set validgit=1
    git log . 1> %temp1% 2>&1
    type %temp1% | find /i /c "Not a git repository" > %temp1c%
    set flag=
    set /p flag=<%temp1c%
    if %flag% == 1 (
      set svn_revision=invalid
      cd %CURDIR%
      set validgit=0
      echo *** warning: %svn_dir% is not a valid git repository
      exit /b 1
    ) 
  )
)

:: get svn revision number

if %validsvn% ==1 (
  svn info 2>&1 | find /i "Last Changed Rev:" | gawk -F" " "{print $4}" > %temp1%
  set /p revision=<%temp1%
)
if %validgit% ==1 (
  git log . 2>&1 | head -1 | gawk -F" " "{print $2}" > %temp1%
  set /p revision=<%temp1%
)

:: get svn date

if %validsvn% ==1 (
  svn info 2>&1 | find /i "Last Changed Date:" | gawk -F" " "{print $4}" > %temp1%
  set /p revision_date=<%temp1%
  svn info 2>&1 | find /i "Last Changed Date:" | gawk -F" " "{print $5}" |gawk -F":" "{print $1\":\"$2}"  > %temp1%
  set /p revision_time=<%temp1%
)
if %validgit% ==1 (
  git log --date=short . 2>&1 | head -3 | tail -1 | gawk -F" " "{print $2}" > %temp1%
  set /p revision_date=<%temp1%
  git log . 2>&1 | head -3 | tail -1 | gawk -F" " "{print $5}" |gawk -F":" "{print $1\":\"$2}"  > %temp1%
  set /p revision_time=<%temp1%
)

:: get current date time

echo %date% 2>&1 | gawk -F" " "{print $2}" | gawk -F"/" "{print $3\"-\"$1\"-\"$2}" > %temp1% 
set /p build_date=<%temp1%
echo %time% 2>&1 | gawk -F":" "{print $1\":\"$2}" > %temp1%
set /p build_time=<%temp1%

cd %CURDIR%

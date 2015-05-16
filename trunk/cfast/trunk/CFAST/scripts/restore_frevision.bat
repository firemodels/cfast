@echo off

:: strip info from keywords Revision, RevisionDate and CompileDate

set bindir=%~p0
set file=%1

if NOT exist %file% (
  exit /b 1
)

call %bindir%\contract_keyword Revision %file%
call %bindir%\contract_keyword RevisionDate %file%
call %bindir%\contract_keyword CompileDate %file%

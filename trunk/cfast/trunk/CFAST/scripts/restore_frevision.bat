@echo off
set file=%1

if NOT exist %file% (
  exit /b 1
)

call ..\scripts\contract_keyword Revision %file%
call ..\scripts\contract_keyword RevisionDate %file%
call ..\scripts\contract_keyword CompileDate %file%

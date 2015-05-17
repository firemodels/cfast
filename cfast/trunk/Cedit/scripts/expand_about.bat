@echo off

:: expand the keyword Revision in About.vb
:: this script must be called in the directory containing About.vb

call ..\CFAST\scripts\get_repo_properties .
call ..\CFAST\scripts\expand_keyword Revision %revision% About.vb

@echo off

:: expand keywords Revision in About.vb
:: this script is called in directory containing About.vb

call ..\CFAST\scripts\get_repo_properties .
call ..\CFAST\scripts\expand_keyword Revision %revision% About.vb

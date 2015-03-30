@echo off

call %SVNROOT%\Validation\scripts\getopts.bat %*

if exist %dir%\%infile%.stop erase %dir%\%infile%.stop
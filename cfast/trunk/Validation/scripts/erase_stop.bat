@echo off

call %SVNROOT%\Validation\scripts\getopts.bat %*

echo erasing %dir%\%infile%.stop
if exist %dir%\%infile%.stop erase %dir%\%infile%.stop
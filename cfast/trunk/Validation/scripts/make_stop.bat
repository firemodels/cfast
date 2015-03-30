@echo off
call %SVNROOT%\Validation\scripts\getopts.bat %*

echo 2 > %dir%\%infile%.stop

@echo off
call %SVNROOT%\Validation\scripts\getopts.bat %*

echo creating stop file: %dir%\%infile%.stop

echo 2 > %dir%\%infile%.stop

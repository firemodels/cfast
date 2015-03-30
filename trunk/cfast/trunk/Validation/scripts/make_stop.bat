@echo off
call %SCRIPT_DIR%\getopts.bat %*

echo creating stop file: %dir%\%infile%.stop

echo 2 > %dir%\%infile%.stop

@echo off
call %SCRIPT_DIR%\getopts.bat %*

echo 2 > %dir%\%infile%.stop

@echo off

call %SCRIPT_DIR%\getopts.bat %*

echo erasing %dir%\%infile%.stop
if exist %dir%\%infile%.stop erase %dir%\%infile%.stop
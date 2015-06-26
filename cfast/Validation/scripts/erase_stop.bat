@echo off

call %SCRIPT_DIR%\getopts.bat %*

if exist %dir%\%infile%.stop erase %dir%\%infile%.stop
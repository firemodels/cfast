@echo off
REM Configure CFAST's environment, then activate it in this Command Prompt.
REM Usage: call setup_python_env.bat [--batchmode]
powershell.exe -NoProfile -File "%~dp0setup_python_env.ps1" %*
if errorlevel 1 exit /b 1
call "%~dp0..\..\.github\cfast_python_env\Scripts\activate.bat"
if errorlevel 1 exit /b 1
set "PYTHONPATH=%~dp0..\..\..\fds\Utilities\Python;%PYTHONPATH%"
exit /b 0

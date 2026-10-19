@echo off
setlocal DisableDelayedExpansion
rem Use a process-only execution policy; do not change the user's saved policy.
"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoLogo -NoProfile -ExecutionPolicy Bypass -File "%~dp0Run_CFAST_Cases.ps1" %*
exit /b %errorlevel%

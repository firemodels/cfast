@echo off

set CURDIR=%CD%

cd ..\..\..
set cfastrepo=%CD%
set copyCFASTcases=%cfastrepo%\Utilities\for_bundle\scripts\copyCFASTcases
set validation_cases=%cfastrepo%\Validation\Scripts\CFAST_Cases.bat

:: make sure sh2bat is installed
call :is_installed sh2bat

::*** create directories

cd %CURDIR%
if exist Examples rmdir /s /q Examples
mkdir Examples
mkdir Examples\Verification
mkdir Examples\Validation

::*** convert Validation case list

cd %cfastrepo%\Validation\scripts
sh2bat CFAST_Cases.sh CFAST_Cases.bat

::*** copying Validation files
echo.
echo --- copying Validation files ---

set "RUNCFAST=call %copyCFASTcases%"
set outdir=%CURDIR%\Examples\Validation

cd %cfastrepo%\Validation
call %validation_cases%

cd %CURDIR%
goto eof

:: -------------------------------------------------------------
:is_file_installed
:: -------------------------------------------------------------

  set program=%1
  %program% --help 1>> is_installed.txt 2>&1
  type is_installed.txt | find /i /c "not recognized" > error_count.txt
  set /p nothave=<error_count.txt
  if %nothave% == 1 (
    echo "***Fatal error: %program% not present"
    echo "installer aborted"
    erase is_installed.txt error_count.txt
    exit /b 1
  )
  exit /b 0

:eof

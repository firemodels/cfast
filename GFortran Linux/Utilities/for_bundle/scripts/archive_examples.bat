@echo off

set CURDIR=%CD%

cd ..\..\..
set cfastrepo=%CD%
set copyCFASTcases=%cfastrepo%\Utilities\for_bundle\scripts\copyCFASTcases
set validation_cases=%cfastrepo%\Validation\Scripts\CFAST_Cases.bat

:: make sure sh2bat is installed
call :is_file_installed sh2bat

:: make sure wzzip is installed
call :is_file_installed wzzip

::*** create directories

cd %CURDIR%
if exist Examples rmdir /s /q Examples
if exist Examples.zip erase Examples.zip
mkdir Examples
mkdir Examples\Verification
mkdir Examples\Validation

::*** convert Validation case list

cd %cfastrepo%\Validation\scripts
sh2bat CFAST_Cases.sh CFAST_Cases.bat


set "RUNCFAST=call %copyCFASTcases%"
set outdir=%CURDIR%\Examples\Validation

cd %cfastrepo%\Validation
echo.
echo *** copying cases
call %validation_cases% > Nul

cd %CURDIR%
echo.
echo *** zipping cases
wzzip -a -r -P Examples.zip Examples > Nul

if exist Examples.zip echo *** The zip file Examples.zip was created
if not exist Examples.zip echo *** The zip file Examples.zip failed to be created

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

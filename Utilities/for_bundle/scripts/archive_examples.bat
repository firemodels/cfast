@echo off

set CURDIR=%CD%

cd ..\..\..
set cfastrepo=%CD%
set sh2batdir=%cfastrepo%\Utilities\sh2bat
set sh2batexe=%sh2batdir%\sh2bat.exe
set copyCFASTcases=%cfastrepo%\Utilities\for_bundle\scripts\copyCFASTcases
set validation_cases=%cfastrepo%\Validation\Scripts\CFAST_Cases.bat
set verification_cases=%cfastrepo%\Verification\Scripts\CFAST_Cases.bat


::*** build sh2bat if it doesn't exist

if exist %sh2batexe% goto skip_build_sh2bat
cd %sh2batdir%
call make_sh2bat nopause
:skip_build_sh2bat

echo sh2batexe=%sh2batexe%
if exist %sh2batexe% goto skip_build_sh2bat2
echo %sh2batexe% does not exist
echo copy examples script abored
exit
:skip_build_sh2bat2

::*** create directories

cd %CURDIR%
if not exist Examples              mkdir Examples
if not exist Examples\Verification mkdir Examples\Verification
if not exist Examples\Validation   mkdir Examples\Validation

::*** convert Validation case list

cd %cfastrepo%\Validation\scripts
%sh2batexe% CFAST_Cases.sh CFAST_Cases.bat

::*** copying Validation files
echo.
echo --- copying Validation files ---

set "RUNCFAST=call %copyCFASTcases%"
set outdir=%CURDIR%\Examples\Validation

cd %cfastrepo%\Validation
call %validation_cases%

::*** convert Verification case list

:: comment out the following until the Veriffication CFAST_Cases.sh script is correct
::cd %cfastrepo%\Verification\scripts
::%sh2batexe% CFAST_Cases.sh CFAST_Cases.bat

::*** copying Verification files
::echo.
::echo --- copying Verification files ---

::set "RUNCFAST=call %copyCFASTcases%"
::set outdir=%CURDIR%\Examples\Verification

::cd %cfastrepo%\Verification
::call %verification_cases%


cd %CURDIR%

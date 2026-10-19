@echo off
setlocal EnableExtensions DisableDelayedExpansion
rem Run CFAST_Cases.sh entries directly with cmd.exe; no PowerShell or Bash.
set "exe="
set "compiler=intel"
set "debug="
set "selected="
set "preview=0"

:parse_args
if "%~1"=="" goto setup
if /i "%~1"=="-e" goto executable_arg
if /i "%~1"=="-I" goto compiler_arg
if /i "%~1"=="-d" goto debug_arg
if /i "%~1"=="-Case" goto case_arg
if /i "%~1"=="-v" goto preview_arg
if /i "%~1"=="-h" goto help
>&2 echo Unknown option: "%~1". Use -h for help.
exit /b 2

:executable_arg
if "%~2"=="" goto missing_argument
set "exe=%~2"
shift /1
shift /1
goto parse_args

:compiler_arg
if "%~2"=="" goto missing_argument
set "compiler=%~2"
shift /1
shift /1
goto parse_args

:case_arg
if "%~2"=="" goto missing_argument
set "selected=%~2"
shift /1
shift /1
goto parse_args

:debug_arg
set "debug=_db"
shift /1
goto parse_args

:preview_arg
set "preview=1"
shift /1
goto parse_args

:missing_argument
>&2 echo Option "%~1" requires a value.
exit /b 2

:setup
if /i not "%compiler%"=="intel" if /i not "%compiler%"=="gnu" (
    >&2 echo Compiler must be intel or gnu.
    exit /b 2
)
for %%R in ("%~dp0..") do set "suite_dir=%%~fR"
for %%R in ("%~dp0..\..") do set "repo=%%~fR"
set "case_list=%~dp0CFAST_Cases.sh"
if not defined exe set "exe=%repo%\Build\CFAST\%compiler%_win%debug%\cfast8_win%debug%.exe"
set "resolved_exe="
if exist "%exe%" if not exist "%exe%\" for %%E in ("%exe%") do set "resolved_exe=%%~fE"
if not defined resolved_exe for %%E in ("%exe%") do set "resolved_exe=%%~$PATH:E"
if not defined resolved_exe if exist "%exe%.exe" for %%E in ("%exe%.exe") do set "resolved_exe=%%~fE"
if not defined resolved_exe for %%E in ("%exe%.exe") do set "resolved_exe=%%~$PATH:E"
if not defined resolved_exe (
    >&2 echo Executable not found: "%exe%"
    >&2 echo Use -e "C:\path\to\cfast.exe", or -I gnu for a standard GNU build.
    exit /b 1
)
if not exist "%case_list%" (
    >&2 echo Case list not found: "%case_list%"
    exit /b 1
)
rem Check the whole selected list before changing any case outputs.
set "mode=check"
set "matched=0"
call :process_list
if errorlevel 1 exit /b %errorlevel%
if "%matched%"=="0" (
    >&2 echo No matching cases: "%selected%"
    exit /b 1
)
echo CFAST executable: "%resolved_exe%"
set "mode=run"
call :process_list
exit /b %errorlevel%

:process_list
for /f "usebackq eol=# tokens=1-4,*" %%A in ("%case_list%") do (
    set "case_command=%%A"
    set "case_option=%%B"
    set "case_dir=%%C"
    set "case_input=%%D"
    set "case_extra=%%E"
    rem Pass values through variables, avoiding CALL's second expansion of paths.
    call :case_entry
    if errorlevel 1 exit /b
)
exit /b 0

:case_entry
if not "%case_command%"=="$RUNCFAST" goto invalid_entry
if not "%case_option%"=="-d" goto invalid_entry
if not defined case_dir goto invalid_entry
if not defined case_input goto invalid_entry
if defined case_extra if not "%case_extra:~0,1%"=="#" goto invalid_entry
for %%N in ("%case_input%") do (
    set "case_stem=%%~nN"
    if /i not "%%~xN"==".in" goto invalid_entry
    if not "%%~nxN"=="%case_input%" goto invalid_entry
)
if defined selected if /i not "%case_input%"=="%selected%" if /i not "%case_stem%"=="%selected%" exit /b 0
set "case_dir=%case_dir:/=\%"
if not exist "%suite_dir%\%case_dir%\%case_input%" (
    >&2 echo Input not found: "%suite_dir%\%case_dir%\%case_input%"
    exit /b 1
)
set /a matched+=1 >nul
if "%mode%"=="check" exit /b 0
if "%preview%"=="1" (
    echo Would run: "%suite_dir%\%case_dir%\%case_input%"
    exit /b 0
)
echo Running locally: "%suite_dir%\%case_dir%\%case_input%"
pushd "%suite_dir%\%case_dir%"
if errorlevel 1 exit /b 1
if exist "%case_stem%.exit" del /q "%case_stem%.exit"
if exist "%case_stem%.stop" del /q "%case_stem%.stop"
if exist "%case_stem%.exit" goto cleanup_failed
if exist "%case_stem%.stop" goto cleanup_failed
rem CFAST writes its own .log; redirect console output to a separate file.
"%resolved_exe%" "%case_input%" >"%case_stem%.stdout" 2>"%case_stem%.err"
set "case_rc=%errorlevel%"
rem Clear ERRORLEVEL so a failed status-file write is distinct from CFAST failure.
ver >nul
>"%case_stem%.exit" echo %case_rc%
if errorlevel 1 set "case_rc=1"
popd
if not "%case_rc%"=="0" >&2 echo Run failed with exit code %case_rc%: "%suite_dir%\%case_dir%\%case_input%". See its .stdout and .err files.
exit /b %case_rc%

:cleanup_failed
>&2 echo Could not remove the previous exit or stop file for "%case_input%".
popd
exit /b 1

:invalid_entry
>&2 echo Unsupported entry in "%case_list%": "%case_command% %case_option% %case_dir% %case_input% %case_extra%"
exit /b 1

:help
echo Usage: Run_CFAST_Cases.bat [options]
echo   -e path       CFAST executable path or name on PATH
echo   -I intel^|gnu  Standard Windows build compiler; default: intel
echo   -d            Select the standard debug build
echo   -Case name    Select matching input filenames or stems
echo   -v            Preview without changing outputs
echo   -h            Show this help
echo Cases run locally in list order and stop on the first failure.
exit /b 0

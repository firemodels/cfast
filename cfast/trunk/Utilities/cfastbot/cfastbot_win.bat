@echo off
set emailto=%1

:: -------------------------------------------------------------
::                         set repository names
:: -------------------------------------------------------------

set fdsbasename=FDS-SMVclean
set cfastbasename=cfastclean

:: -------------------------------------------------------------
::                         setup environment
:: -------------------------------------------------------------

set CURDIR=%CD%

if not exist output mkdir output
if not exist history mkdir history
if not exist timings mkdir timings

set OUTDIR=%CURDIR%\output
set HISTORYDIR=%CURDIR%\history
set TIMINGSDIR=%CURDIR%\timings

erase %OUTDIR%\*.txt 1> Nul 2>&1

set cfastsvnroot=%userprofile%\%cfastbasename%
set FDSsvnroot=%userprofile%\%fdsbasename%

set email=%FDSsvnroot%\SMV\scripts\email.bat
set emailexe=%userprofile%\bin\mailsend.exe

set errorlog=%OUTDIR%\stage_errors.txt
set warninglog=%OUTDIR%\stage_warnings.txt
set errorwarninglog=%OUTDIR%\stage_errorswarnings.txt
set infofile=%OUTDIR%\stage_info.txt
set revisionfilestring=%OUTDIR%\revision.txt
set revisionfilenum=%OUTDIR%\revision_num.txt
set stagestatus=%OUTDIR%\stage_status.log

set haveerrors=0
set havewarnings=0

set gettimeexe=%FDSsvnroot%\Utilities\get_time\intel_win_64\get_time.exe
set runbatchexe=%FDSsvnroot%\SMV\source\runbatch\intel_win_64\runbatch.exe

date /t > %OUTDIR%\starttime.txt
set /p startdate=<%OUTDIR%\starttime.txt
time /t > %OUTDIR%\starttime.txt
set /p starttime=<%OUTDIR%\starttime.txt

call "%cfastsvnroot%\scripts\setup_intel_compilers.bat" 1> Nul 2>&1
call "%cfastsvnroot%\Utilities\cfastbot\cfastbot_email_list.bat" 1> Nul 2>&1
if NOT "%emailto%" == "" (
  set mailToCFAST=%emailto%
)

:: -------------------------------------------------------------
::                           stage 0 - preliminaries
:: -------------------------------------------------------------

echo Stage 0 - Preliminaries

::*** check if various software is installed

echo. > %errorlog%
echo. > %warninglog%
echo. > %stagestatus%

::*** looking for gettime

call :is_file_installed %gettimeexe%|| exit /b 1
echo             found get_time

call :GET_TIME
set TIME_beg=%current_time%

call :GET_TIME
set PRELIM_beg=%current_time% 

::*** looking for fortran

ifort 1> %OUTDIR%\stage0a.txt 2>&1
type %OUTDIR%\stage0a.txt | find /i /c "not recognized" > %OUTDIR%\stage_count0a.txt
set /p nothaveFORTRAN=<%OUTDIR%\stage_count0a.txt
if %nothaveFORTRAN% == 1 (
  echo "***Fatal error: Fortran compiler not present"
  echo "***Fatal error: Fortran compiler not present" > %errorlog%
  echo "cfastbot run aborted"
  call :output_abort_message
  exit /b 1
)
echo             found Fortran

::*** looking for C

icl 1> %OUTDIR%\stage0a.txt 2>&1
type %OUTDIR%\stage0a.txt | find /i /c "not recognized" > %OUTDIR%\stage_count0a.txt
set /p nothaveICC=<%OUTDIR%\stage_count0a.txt

if %nothaveICC% == 0 (
  echo             found C
)
if %nothaveICC% == 1 (
  call :is_file_installed smokeview|| exit /b 1
  echo             found smokeview (C/C++ not available)
  set smokeview=smokeview.exe
)

::*** looking for email

if NOT exist %emailexe% (
  echo ***warning: email client not found.   
  echo             cfastbot messages will only be sent to the console.
) else (
  echo             found mailsend
)

::*** looking for pdflatex

call :is_file_installed pdflatex|| exit /b 1
echo             found pdflatex

::*** looking for grep

call :is_file_installed grep|| exit /b 1
echo             found grep

::*** looking for cut

call :is_file_installed cut|| exit /b 1
echo             found cut

::*** looking for sed

call :is_file_installed sed|| exit /b 1
echo             found sed

:: --------------------setting up repositories ------------------------------

::*** revert cfast repository

if "%cfastbasename%" == "cfastclean" (
   echo             reverting %cfastbasename% repository
   cd %cfastsvnroot%
   call :svn_revert 1> Nul 2>&1
)

::*** update cfast repository

echo             updating %cfastbasename% repository
cd %cfastsvnroot%
svn update  1> %OUTDIR%\stage0.txt 2>&1

svn info | grep Revision > %revisionfilestring%
set /p revisionstring=<%revisionfilestring%

svn info | grep Revision | cut -d " " -f 2 > %revisionfilenum%
set /p revisionnum=<%revisionfilenum%

set errorlogpc=%HISTORYDIR%\errors_%revisionnum%.txt
set warninglogpc=%HISTORYDIR%\warnings_%revisionnum%.txt

set timingslogfile=%TIMINGSDIR%\timings_%revisionnum%.txt

::*** revert FDS repository

if "%FDSbasename%" == "FDS-SMVclean" (
   echo             reverting %FDSbasename% repository
   cd %FDSsvnroot%
   call :svn_revert 1> Nul 2>&1
)

::*** update FDS repository

echo             updating %FDSbasename% repository
cd %FDSsvnroot%
svn update  1> %OUTDIR%\stage0.txt 2>&1

call :GET_TIME
set PRELIM_end=%current_time%
call :GET_DURATION PRELIM %PRELIM_beg% %PRELIM_end%
set DIFF_PRELIM=%duration%

:: -------------------------------------------------------------
::                           stage 1 - build cfast
:: -------------------------------------------------------------

call :GET_TIME
set BUILDFDS_beg=%current_time% 
echo Stage 1 - Building CFAST

echo             debug

cd %cfastsvnroot%\CFAST\intel_win_64_db
erase *.obj *.mod *.exe *.pdb *.optrpt 1> %OUTDIR%\stage1a.txt 2>&1
make VPATH="../Source:../Include" INCLUDE="../Include" -f ..\makefile intel_win_64_db 1>> %OUTDIR%\stage1a.txt 2>&1


call :does_file_exist cfast7_win_64_db.exe %OUTDIR%\stage1a.txt|| exit /b 1

call :find_cfast_warnings "warning" %OUTDIR%\stage1a.txt "Stage 1a"

echo             release

cd %cfastsvnroot%\CFAST\intel_win_64
erase *.obj *.mod *.exe *.pdb *.optrpt 1> %OUTDIR%\stage1b.txt 2>&1
make VPATH="../Source:../Include" INCLUDE="../Include"  -f ..\makefile intel_win_64 1>> %OUTDIR%\stage1b.txt 2>&1

call :does_file_exist cfast7_win_64.exe %OUTDIR%\stage1b.txt|| exit /b 1
call :find_cfast_warnings "warning" %OUTDIR%\stage1b.txt "Stage 1b"

call :GET_TIME
set BUILDFDS_end=%current_time%
call :GET_DURATION BUILDFDS %BUILDFDS_beg% %BUILDFDS_end%
set DIFF_BUILDFDS=%duration%

:: -------------------------------------------------------------
::                           stage 2 - build smokeview
:: -------------------------------------------------------------

if %nothaveICC% == 1 (
  goto skip_stage2
)
call :GET_TIME
set BUILDSMVUTIL_beg=%current_time% 
echo Stage 2 - Building Smokeview

echo             libs

cd %FDSsvnroot%\SMV\Build\LIBS\lib_win_intel_64
call makelibs2 1>> %OUTDIR%\stage2a.txt 2>&1

echo             debug

cd %FDSsvnroot%\SMV\Build\intel_win_64
erase *.obj *.mod *.exe smokeview_win_64_db.exe 1> %OUTDIR%\stage2a.txt 2>&1
make -f ..\Makefile intel_win_64_db 1>> %OUTDIR%\stage2a.txt 2>&1

call :does_file_exist smokeview_win_64_db.exe %OUTDIR%\stage2a.txt|| exit /b 1
call :find_smokeview_warnings "warning" %OUTDIR%\stage2a.txt "Stage 2a"

echo             release

cd %FDSsvnroot%\SMV\Build\intel_win_64
erase *.obj *.mod smokeview_win_64.exe 1> %OUTDIR%\stage2b.txt 2>&1
make -f ..\Makefile intel_win_64 1>> %OUTDIR%\stage2b.txt 2>&1
set smokeview=%FDSsvnroot%\SMV\Build\intel_win_64\smokeview_win_64.exe

call :does_file_exist smokeview_win_64.exe %OUTDIR%\stage2b.txt|| exit /b 1
call :find_smokeview_warnings "warning" %OUTDIR%\stage2b.txt "Stage 2b"
:skip_stage2

:: -------------------------------------------------------------
::                           stage 4 - run cases
:: -------------------------------------------------------------

call :GET_TIME
set RUNVV_beg=%current_time% 

echo Stage 4 - Running validation cases
echo             debug

cd %cfastsvnroot%\Validation\scripts

call Run_CFAST_cases 1 1> %OUTDIR%\stage4a.txt 2>&1

call :find_smokeview_warnings "error" %OUTDIR%\stage4a.txt "Stage 4a_1"
call :find_smokeview_warnings "forrtl: severe" %OUTDIR%\stage4a.txt "Stage 4a_2"

echo             release

cd %cfastsvnroot%\Validation\scripts

call Run_CFAST_cases 1> %OUTDIR%\stage4b.txt 2>&1

call :find_smokeview_warnings "error" %OUTDIR%\stage4b.txt "Stage 4b_1"
call :find_smokeview_warnings "forrtl: severe" %OUTDIR%\stage4b.txt "Stage 4b_2"

call :GET_TIME
set RUNVV_end=%current_time% 
call :GET_DURATION RUNVV %RUNVV_beg% %RUNVV_end%
set DIFF_RUNVV=%duration%

:: -------------------------------------------------------------
::                           stage 5 - make pictures
:: -------------------------------------------------------------

call :GET_TIME
set MAKEPICS_beg=%current_time% 
echo Stage 5 - Making pictures for cfast cases

cd %cfastsvnroot%\Validation\scripts
set SH2BAT=%cfastsvnroot%\Validation\scripts\sh2bat.exe

%SH2BAT% CFAST_Pictures.sh CFAST_Pictures.bat > %OUTDIR%\stage5.txt 2>&1
set RUNCFAST=call %cfastsvnroot%\Validation\scripts\runsmv.bat

cd %cfastsvnroot%\Validation
set BASEDIR=%CD%

call scripts\CFAST_Pictures.bat 1> %OUTDIR%\stage5.txt 2>&1

call :GET_TIME
set MAKEPICS_end=%current_time% 
call :GET_DURATION MAKEPICS %MAKEPICS_beg% %MAKEPICS_end%
set DIFF_MAKEPICS=%duration%

:: -------------------------------------------------------------
::                           stage 6 - make manuals
:: -------------------------------------------------------------

call :GET_TIME
set MAKEGUIDES_beg=%current_time% 
echo Stage 6 - Building CFAST guides

echo             Users Guide
call :build_guide Users_Guide %cfastsvnroot%\Docs\Users_Guide 1>> %OUTDIR%\stage6.txt 2>&1

echo             Technical Reference Guide
call :build_guide Tech_Ref %cfastsvnroot%\Docs\Tech_Ref 1>> %OUTDIR%\stage6.txt 2>&1

::echo             Validation Guide
::call :build_guide Validation_Guide %cfastsvnroot%\Docs\Validation_Guide 1>> %OUTDIR%\stage6.txt 2>&1

call :GET_TIME
set MAKEGUIDES_end=%current_time%
call :GET_DURATION MAKEGUIDES %MAKEGUIDES_beg% %MAKEGUIDES_end%
set DIFF_MAKEGUIDES=%duration%

:: -------------------------------------------------------------
::                           wrap up
:: -------------------------------------------------------------

date /t > %OUTDIR%\stoptime.txt
set /p stopdate=<%OUTDIR%\stoptime.txt
time /t > %OUTDIR%\stoptime.txt
set /p stoptime=<%OUTDIR%\stoptime.txt

echo. > %infofile%
echo . ----------------------------- >> %infofile%
echo .         host: %COMPUTERNAME% >> %infofile%
echo .        start: %startdate% %starttime% >> %infofile%
echo .         stop: %stopdate% %stoptime%  >> %infofile%
echo .    run cases: %DIFF_RUNVV% >> %infofile%
echo .make pictures: %DIFF_MAKEPICS% >> %infofile%
echo .        total: %DIFF_TIME% >> %infofile%
echo . ----------------------------- >> %infofile%

copy %infofile% %timingslogfile%

:: email results

sed "s/$/\r/" < %warninglog% > %warninglogpc%
sed "s/$/\r/" < %errorlog% > %errorlogpc%

if exist %emailexe% (
  if %havewarnings% == 0 (
    if %haveerrors% == 0 (
      call %email% %mailToCFAST% "cfastbot success on %COMPUTERNAME%! %revisionstring%" %infofile%
    ) else (
      echo. >> %infofile%
      type %errorlogpc% >> %infofile%
      call %email% %mailToCFAST% "cfastbot failure on %COMPUTERNAME%! %revisionstring%" %infofile%
    )
  ) else (
    if %haveerrors% == 0 (
      echo. >> %infofile%
      type %warninglogpc% >> %infofile%
      %email% %mailToCFAST% "cfastbot success with warnings on %COMPUTERNAME% %revisionstring%" %infofile%
    ) else (
      echo. >> %infofile%
      type %errorlogpc% >> %infofile%
      echo. >> %infofile%
      type %warninglogpc% >> %infofile%
      call %email% %mailToCFAST% "cfastbot failure on %COMPUTERNAME%! %revisionstring%" %infofile%
    )
  )
)

goto eof

:output_abort_message
  echo "***Fatal error: cfastbot build failure on %COMPUTERNAME% %revision%"
exit /b

:: -------------------------------------------------------------
:GET_TIME
:: -------------------------------------------------------------

%gettimeexe% > time.txt
set /p current_time=<time.txt
exit /b 0

:: -------------------------------------------------------------
:GET_DURATION
:: -------------------------------------------------------------
set /a difftime=%3 - %2
set /a diff_h= %difftime%/3600
set /a diff_m= (%difftime% %% 3600 )/60
set /a diff_s= %difftime% %% 60
if %difftime% GEQ 3600 set duration= %diff_h%h %diff_m%m %diff_s%s
if %difftime% LSS 3600 if %difftime% GEQ 60 set duration= %diff_m%m %diff_s%s
if %difftime% LSS 3600 if %difftime% LSS 60 set duration= %diff_s%s
echo %1: %duration% >> %stagestatus%
exit /b 0

:: -------------------------------------------------------------
:is_file_installed
:: -------------------------------------------------------------

  set program=%1
  %program% -help 1>> %OUTDIR%\stage_exist.txt 2>&1
  type %OUTDIR%\stage_exist.txt | find /i /c "not recognized" > %OUTDIR%\stage_count.txt
  set /p nothave=<%OUTDIR%\stage_count.txt
  if %nothave% == 1 (
    echo "***Fatal error: %program% not present"
    echo "***Fatal error: %program% not present" > %errorlog%
    echo "cfastbot run aborted"
    call :output_abort_message
    exit /b 1
  )
  exit /b 0

:: -------------------------------------------------------------
  :does_file_exist
:: -------------------------------------------------------------

set file=%1
set outputfile=%2

if NOT exist %file% (
  echo ***fatal error: problem building %file%. Aborting cfastbot
  type %outputfile% >> %errorlog%
  call :output_abort_message
  exit /b 1
)
exit /b 0

:: -------------------------------------------------------------
  :find_smokeview_warnings
:: -------------------------------------------------------------

set search_string=%1
set search_file=%2
set stage=%3

grep -v "commands for target" %search_file% > %OUTDIR%\stage_warning0.txt
grep -i -A 5 -B 5 %search_string% %OUTDIR%\stage_warning0.txt > %OUTDIR%\stage_warning.txt
type %OUTDIR%\stage_warning.txt | find /v /c "kdkwokwdokwd"> %OUTDIR%\stage_nwarning.txt
set /p nwarnings=<%OUTDIR%\stage_nwarning.txt
if %nwarnings% GTR 0 (
  echo %stage% warnings >> %warninglog%
  echo. >> %warninglog%
  type %OUTDIR%\stage_warning.txt >> %warninglog%
  set havewarnings=1
)
exit /b

:: -------------------------------------------------------------
  :find_cfast_warnings
:: -------------------------------------------------------------

set search_string=%1
set search_file=%2
set stage=%3

grep -v "mpif.h" %search_file% > %OUTDIR%\stage_warning0.txt
grep -i -A 5 -B 5 %search_string% %OUTDIR%\stage_warning0.txt  > %OUTDIR%\stage_warning.txt
type %OUTDIR%\stage_warning.txt | find /c ":"> %OUTDIR%\stage_nwarning.txt
set /p nwarnings=<%OUTDIR%\stage_nwarning.txt
if %nwarnings% GTR 0 (
  echo %stage% warnings >> %warninglog%
  echo. >> %warninglog%
  type %OUTDIR%\stage_warning.txt >> %warninglog%
  set havewarnings=1
)
exit /b

:: -------------------------------------------------------------
:svn_revert
:: -------------------------------------------------------------
svn cleanup .
svn revert -R .
For /f "tokens=1,2" %%A in ('svn status --no-ignore') Do (
     If [%%A]==[?] ( Call :UniDelete %%B
     ) Else If [%%A]==[I] Call :UniDelete %%B
   )
exit /b

:: -------------------------------------------------------------
:UniDelete delete file/dir
:: -------------------------------------------------------------
if "%1"=="%~nx0" exit /b
IF EXIST "%1\*" ( 
    RD /S /Q "%1"
) Else (
    If EXIST "%1" DEL /S /F /Q "%1"
)
exit /b


:: -------------------------------------------------------------
 :build_guide
:: -------------------------------------------------------------

set guide=%1
set guide_dir=%2

set guideout=%OUTDIR%\stage6_%guide%.txt

cd %guide_dir%

pdflatex -interaction nonstopmode %guide% 1> %guideout% 2>&1
bibtex %guide% 1> %guideout% 2>&1
pdflatex -interaction nonstopmode %guide% 1> %guideout% 2>&1
pdflatex -interaction nonstopmode %guide% 1> %guideout% 2>&1
bibtex %guide% 1>> %guideout% 2>&1

type %guideout% | find "Undefined control" > %OUTDIR%\stage_error.txt
type %guideout% | find "! LaTeX Error:" >> %OUTDIR%\stage_error.txt
type %guideout% | find "Fatal error" >> %OUTDIR%\stage_error.txt
type %guideout% | find "Error:" >> %OUTDIR%\stage_error.txt

type %OUTDIR%\stage_error.txt | find /v /c "JDIJWIDJIQ"> %OUTDIR%\stage_nerrors.txt
set /p nerrors=<%OUTDIR%\stage_nerrors.txt
if %nerrors% GTR 0 (
  echo Errors from Stage 6 - Build %guide% >> %errorlog%
  type %OUTDIR%\stage_error.txt >> %errorlog%
  set haveerrors=1
)

type %guideout% | find "undefined" > %OUTDIR%\stage_warning.txt
type %guideout% | find "multiply"  >> %OUTDIR%\stage_warning.txt

type %OUTDIR%\stage_warning.txt | find /c ":"> %OUTDIR%\nwarnings.txt
set /p nwarnings=<%OUTDIR%\nwarnings.txt
if %nwarnings% GTR 0 (
  echo Warnings from Stage 6 - Build %guide% >> %warninglog%
  type %OUTDIR%\stage_warning.txt >> %warninglog%
  set havewarnings=1
)

exit /b

:eof
echo cfastbot_win completed
cd %CURDIR%

@echo off
:: cfastbot_win.bat %cfastrepo% %fdsrepo% %usematlab% %clean% %update% %emailto%

set arg1=%1
set cfastroot=%~f1
set cfastbasename=%~n1

set arg2=%2
set FDSroot=%2
set fdsbasename=%2

set usematlab=%3
set clean=%4
set update=%5
set installed=%6
set skip_cases=%7
set official=%8
set emailto=%9

set size=_64

:: -------------------------------------------------------------
::                         set repository names
:: -------------------------------------------------------------

:: check for cfast repo

if NOT exist %cfastroot% (
  echo ***error: the repo %cfastroot% does not exist
  echo cfastbot aborted
  exit /b
)

:: check for FDS repo (if specified)

set havefdsrepo=1
if %arg2% == none (
  set havefdsrepo=0
) else (
  set FDSroot=%~f2
  set fdsbasename=%~n2
  if not exist %FDSroot% (
    set havefdsrepo=0
    echo ***warning: the repo %FDSroot% does not exist  
  )
)

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
set timefile=%OUTDIR%\time.txt


erase %OUTDIR%\*.txt 1> Nul 2>&1

set email=%cfastroot%\Utilities\scripts\email.bat
set emailexe=%userprofile%\bin\mailsend.exe

set errorlog=%OUTDIR%\stage_errors.txt
set warninglog=%OUTDIR%\stage_warnings.txt
set matlabverlog=%OUTDIR%\matlab_ver_log.txt
set matlabvallog=%OUTDIR%\matlab_val_log.txt
set errorwarninglog=%OUTDIR%\stage_errorswarnings.txt
set infofile=%OUTDIR%\summary.txt
set revisionfilestring=%OUTDIR%\revision.txt
set revisionfilenum=%OUTDIR%\revision_num.txt
set stagestatus=%OUTDIR%\stage_status.log
set nothaveValidation=0
set nothaveICC=1

set haveerrors=0
set havewarnings=0

set gettimeexe=%userprofile%\FIRE-LOCAL\repo_exes\get_time.exe
set runbatchexe=%userprofile%\FIRE-LOCAL\repo_exes\runbatch.exe

date /t > %OUTDIR%\starttime.txt
set /p startdate=<%OUTDIR%\starttime.txt
time /t > %OUTDIR%\starttime.txt
set /p starttime=<%OUTDIR%\starttime.txt

call %cfastroot%\Build\scripts\setup_intel_compilers.bat 1> Nul 2>&1
call %cfastroot%\Utilities\cfastbot\cfastbot_email_list.bat 1> Nul 2>&1

set usematlab=%3
if %usematlab% == 1 (
  echo matlab: using matlab scripts
)
if %usematlab% == 0 (
  echo matlab: using prebuilt matlab executables
)
if NOT "%emailto%" == "" (
  echo  email: %emailto%
  set mailToCFAST=%emailto%
)
set version=test
if %official% == 1 (
  set version=release
)

echo cfast repo: %cfastroot%
echo   FDS repo: %FDSroot%
:: -------------------------------------------------------------
::                           stage 0 - preliminaries
:: -------------------------------------------------------------

echo Stage 0 - Preliminaries

::*** check if various software is installed

echo. > %errorlog%
echo log > %warninglog%
echo. > %stagestatus%

::*** looking for gettime

call :is_file_installed %gettimeexe%|| exit /b 1
echo             found get_time

call :GET_TIME TIME_beg
call :GET_TIME PRELIM_beg

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

if %installed% == 1 goto skip_icc
icl 1> %OUTDIR%\stage0a.txt 2>&1
type %OUTDIR%\stage0a.txt | find /i /c "not recognized" > %OUTDIR%\stage_count0a.txt
set /p nothaveICC=<%OUTDIR%\stage_count0a.txt

if %nothaveICC% == 0 (
  echo             found C
)
:skip_icc

if %havefdsrepo% == 0 goto skip1
if %nothaveICC% == 0 goto skip1
  call :is_file_installed smokeview|| exit /b 1
  echo             found smokeview
  set smokeview=smokeview.exe
:skip1

if %havefdsrepo% == 1 goto skip2
  call :is_file_installed smokeview|| exit /b 1
  echo             found smokeview
  set smokeview=smokeview.exe
:skip2

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

::*** looking for head

call :is_file_installed head|| exit /b 1
echo             found head

::*** looking for gawk

call :is_file_installed gawk|| exit /b 1
echo             found gawk

if %usematlab% == 0 goto skip_matlab

::*** looking for matlab

  where matlab 2>&1 | find /i /c "Could not find" > %OUTDIR%\stage_count0a.txt
  set /p nothavematlab=<%OUTDIR%\stage_count0a.txt
  if %nothavematlab% == 0 (
    echo             found matlab
  )
  if %nothavematlab% == 1 (
    echo             matlab not found - looking for executables
    set usematlab=0
  )
:skip_matlab

if %usematlab% == 1 goto skip_matlabexe
::*** looking for Validation

  where Validation_Script 2>&1 | find /i /c "Could not find" > %OUTDIR%\stage_count0a.txt
  set /p nothaveValidation=<%OUTDIR%\stage_count0a.txt
  if %nothaveValidation% == 0 (
    echo             found Validation plot generator 
  )
  if %nothaveValidation% == 1 (
    echo             Validation plot generator not found - Validation guide will not be built 
  )

::*** looking for Verification

  where Verification_Script 2>&1 | find /i /c "Could not find" > %OUTDIR%\stage_count0a.txt
  set /p nothaveVerification=<%OUTDIR%\stage_count0a.txt
  if %nothaveVerification% == 0 (
    echo             found Verification plot generator 
  )
  if %nothaveVerification% == 1 (
    echo             Verification plot generator not found - Validation guide will not be built
    set nothaveValidation=1
  )

:skip_matlabexe

:: --------------------setting up repositories ------------------------------

::*** revert cfast repository

if %clean% == 0 goto skip_update0
   echo             cleaning %cfastbasename% repository
   call :git_clean %cfastroot%\Source\Build
   call :git_clean %cfastroot%\Source\CFAST
   call :git_clean %cfastroot%\Verification
   call :git_clean %cfastroot%\Validation
   call :git_clean %cfastroot%\Manuals
:skip_update0

::*** update cfast repository

if %update% == 0 goto skip_update1
  echo             updating %cfastbasename% repository
  cd %cfastroot%
  git fetch origin
  git pull  1> %OUTDIR%\stage0.txt 2>&1
:skip_update1

cd %cfastroot%
git log --abbrev-commit . | head -1 | gawk "{print $2}" > %revisionfilestring%

set /p revisionstring=<%revisionfilestring%

git log --abbrev-commit . | head -1 | gawk "{print $2}" > %revisionfilenum%
set /p revisionnum=<%revisionfilenum%

set errorlogpc=%HISTORYDIR%\errors_%revisionnum%.txt
set warninglogpc=%HISTORYDIR%\warnings_%revisionnum%.txt

set timingslogfile=%TIMINGSDIR%\timings_%revisionnum%.txt

::*** revert FDS repository

if %havefdsrepo% == 0 goto skip_fdsrepo
  if %clean% == 0 goto skip_update2
    echo             cleaning %FDSbasename% repository
    call :git_clean %FDSroot%\Verification
    call :git_clean %FDSroot%\SMV\source
    call :git_clean %FDSroot%\SMV\Build
    call :git_clean %FDSroot%\FDS_Source
    call :git_clean %FDSroot%\FDS_Compilation
    call :git_clean %FDSroot%\Manuals
  :skip_update2

::*** update FDS repository

  if %update% == 0 goto skip_update3
    echo             updating %FDSbasename% repository
    cd %FDSroot%
    git fetch origin
    git pull  1> %OUTDIR%\stage0.txt 2>&1
  :skip_update3
:skip_fdsrepo

:: -------------------------------------------------------------
::                           stage 1 - build cfast
:: -------------------------------------------------------------

echo Stage 1 - Building CFAST and VandV_Calcs

echo             debug cfast

cd %cfastroot%\Build\CFAST\intel_win%size%_db
erase *.obj *.mod *.exe *.pdb *.optrpt 1> %OUTDIR%\stage1a.txt 2>&1
call make_cfast bot %version% 1>> %OUTDIR%\stage1a.txt 2>&1


call :does_file_exist cfast7_win%size%_db.exe %OUTDIR%\stage1a.txt|| exit /b 1

call :find_cfast_warnings "warning" %OUTDIR%\stage1a.txt "Stage 1a"

echo             release cfast

cd %cfastroot%\Build\CFAST\intel_win%size%
erase *.obj *.mod *.exe *.pdb *.optrpt 1> %OUTDIR%\stage1b.txt 2>&1
call make_cfast bot %version% 1>> %OUTDIR%\stage1b.txt 2>&1

call :does_file_exist cfast7_win%size%.exe %OUTDIR%\stage1b.txt|| exit /b 1
call :find_cfast_warnings "warning" %OUTDIR%\stage1b.txt "Stage 1b"

echo             release VandV_Calcs

cd %cfastroot%\Build\VandV_Calcs\intel_win%size%
erase *.obj *.mod *.exe *.pdb *.optrpt 1> %OUTDIR%\stage1c.txt 2>&1
call make_vv bot 1>> %OUTDIR%\stage1c.txt 2>&1

call :does_file_exist VandV_Calcs_win%size%.exe %OUTDIR%\stage1c.txt|| exit /b 1
call :find_cfast_warnings "warning" %OUTDIR%\stage1c.txt "Stage 1c"

:: -------------------------------------------------------------
::                           stage 2 - build smokeview
:: -------------------------------------------------------------

if %installed% == 1 (
  echo Stage 2 - Skipping Smokeview build
  goto skip_stage2
)
if %nothaveICC% == 1 (
  echo Stage 2 - Skipping Smokeview build - C/C++ not available
  goto skip_stage2
)
if %havefdsrepo% == 0 (
  echo Stage 2 - Skipping Smokeview build - source not available
  goto skip_stage2
)

echo Stage 2 - Building Smokeview

echo             libs

cd %FDSroot%\SMV\Build\LIBS\lib_win_intel%size%
call makelibs bot 1>> %OUTDIR%\stage2a.txt 2>&1

echo             debug

cd %FDSroot%\SMV\Build\intel_win%size%
erase *.obj *.mod *.exe smokeview_win%size%_db.exe 1> %OUTDIR%\stage2a.txt 2>&1
call make_smv_db -r bot 1>> %OUTDIR%\stage2a.txt 2>&1

call :does_file_exist smokeview_win%size%_db.exe %OUTDIR%\stage2a.txt|| exit /b 1
call :find_smokeview_warnings "warning" %OUTDIR%\stage2a.txt "Stage 2a"

echo             release

cd %FDSroot%\SMV\Build\intel_win%size%
erase *.obj *.mod smokeview_win%size%.exe 1> %OUTDIR%\stage2b.txt 2>&1
call make_smv -r bot 1>> %OUTDIR%\stage2b.txt 2>&1
set smokeview=%FDSroot%\SMV\Build\intel_win%size%\smokeview_win%size%.exe

call :does_file_exist smokeview_win%size%.exe %OUTDIR%\stage2b.txt|| exit /b 1
call :find_smokeview_warnings "warning" %OUTDIR%\stage2b.txt "Stage 2b"
:skip_stage2

if %skip_cases% == 1 goto skip_cases

call :GET_DURATION PRELIM %PRELIM_beg%

:: -------------------------------------------------------------
::                           stage 3 - run cases
:: -------------------------------------------------------------

call :GET_TIME RUNVV_beg

echo Stage 3 - Running validation cases
echo             debug

cd %cfastroot%\Validation\scripts

call Run_CFAST_cases 1 1> %OUTDIR%\stage3a.txt 2>&1

call :find_runcases_warnings "***Warning" %cfastroot%\Validation   "Stage 3a-Validation"
call :find_runcases_errors "error|forrtl: severe|DASSL|floating invalid" %cfastroot%\Validation   "Stage 3a-Validation"
call :find_runcases_errors "error|forrtl: severe|DASSL|floating invalid" %cfastroot%\Verification "Stage 3a-Verification"

if %clean% == 1 (
   echo             removing debug output files
   call :git_clean %cfastroot%\Validation
)

echo             release

cd %cfastroot%\Validation\scripts

call Run_CFAST_cases 1> %OUTDIR%\stage3b.txt 2>&1

call :find_runcases_warnings "***Warning" %cfastroot%\Validation   "Stage 3b-Validation"
call :find_runcases_errors "error|forrtl: severe|DASSL|floating invalid" %cfastroot%\Validation   "Stage 3b-Validation"
call :find_runcases_errors "error|forrtl: severe|DASSL|floating invalid" %cfastroot%\Verification "Stage 3b-Verification"

call :GET_DURATION RUNVV %RUNVV_beg%

:: -------------------------------------------------------------
::                           stage 4 - make pictures
:: -------------------------------------------------------------

:skip_cases
call :GET_TIME MAKEPICS_beg

echo Stage 4 - Making smokeview images

cd %cfastroot%\Validation\scripts
set SH2BAT=%userprofile%\FIRE-LOCAL\repo_exes\sh2bat.exe

%SH2BAT% CFAST_Pictures.sh CFAST_Pictures.bat > %OUTDIR%\stage4.txt 2>&1
set RUNSMV=call %cfastroot%\Validation\scripts\runsmv.bat

cd %cfastroot%\Validation
set BASEDIR=%CD%

call scripts\CFAST_Pictures.bat 1> %OUTDIR%\stage4.txt 2>&1

:: -------------------------------------------------------------
::                           stage 5 - make validation plots
:: -------------------------------------------------------------

if %nothaveValidation% == 1 goto skip_stage5

echo Stage 5 - Making matlab plots
::*** generating Validation plots

echo             Validation
echo               VandV_Calcs
cd %cfastroot%\Validation
..\Build\VandV_Calcs\intel_win%size%\VandV_Calcs_win%size%.exe CFAST_Pressure_Correction_Inputs.csv 1> Nul 2>&1
copy pressures.csv LLNL_Enclosure\LLNL_pressures.csv /Y 1> Nul 2>&1
..\Build\VandV_Calcs\\intel_win%size%\VandV_Calcs_win%size%.exe CFAST_Temperature_Profile_inputs.csv 1> Nul 2>&1
copy profiles.csv Steckler_Compartment /Y 1> Nul 2>&1
..\Build\VandV_Calcs\\intel_win%size%\VandV_Calcs_win%size%.exe CFAST_Heat_Flux_Profile_inputs.csv 1> Nul 2>&1
copy flux_profiles.csv Fleury_Heat_Flux /Y 1> Nul 2>&1



echo               Making plots
cd %cfastroot%\Utilities\Matlab

if %usematlab% == 0 goto matlab_else1
  matlab -logfile %matlabvallog% -automation -wait -noFigureWindows -r "try; run('%cfastroot%\Utilities\Matlab\CFAST_validation_script.m'); catch; end; quit
  goto matlab_end1
:matlab_else1
  Validation_Script
  call :WAIT_RUN Validation_Script
:matlab_end1

::*** generating Verification plots

echo             Verification
echo               Making plots
cd %cfastroot%\Utilities\Matlab
if %usematlab% == 0 goto matlab_else2
  matlab -logfile %matlabverlog% -automation -wait -noFigureWindows -r "try; run('%cfastroot%\Utilities\Matlab\CFAST_verification_script.m'); catch; end; quit
  goto matlab_end2
:matlab_else2
  Verification_Script
  call :WAIT_RUN Verification_Script
:matlab_end2

:skip_stage5

call :GET_DURATION MAKEPICS %MAKEPICS_beg%

:: -------------------------------------------------------------
::                           stage 6 - make manuals
:: -------------------------------------------------------------

call :GET_TIME MAKEGUIDES_beg

echo Stage 6 - Building CFAST guides

echo             Users Guide
call :build_guide Users_Guide %cfastroot%\Manuals\Users_Guide 1>> %OUTDIR%\stage6.txt 2>&1

echo             Technical Reference Guide
call :build_guide Tech_Ref %cfastroot%\Manuals\Tech_Ref 1>> %OUTDIR%\stage6.txt 2>&1

if %nothaveValidation% == 0 (
  echo             Validation Guide
  call :build_guide Validation_Guide %cfastroot%\Manuals\Validation_Guide 1>> %OUTDIR%\stage6.txt 2>&1
)

echo             Configuration Management Guide
call :build_guide Configuration_Guide %cfastroot%\Manuals\Configuration_Guide 1>> %OUTDIR%\stage6.txt 2>&1

call :GET_DURATION MAKEGUIDES %MAKEGUIDES_beg%
call :GET_DURATION TOTALTIME %TIME_beg%

:: -------------------------------------------------------------
::                           wrap up
:: -------------------------------------------------------------

date /t > %OUTDIR%\stoptime.txt
set /p stopdate=<%OUTDIR%\stoptime.txt
time /t > %OUTDIR%\stoptime.txt
set /p stoptime=<%OUTDIR%\stoptime.txt

echo. > %infofile%
echo .------------------------------         >> %infofile%
echo .         host: %COMPUTERNAME%          >> %infofile%
echo .        start: %startdate% %starttime% >> %infofile%
echo .         stop: %stopdate% %stoptime%   >> %infofile%
echo .        setup: %DIFF_PRELIM%           >> %infofile%
echo .    run cases: %DIFF_RUNVV%            >> %infofile%
echo .make pictures: %DIFF_MAKEPICS%         >> %infofile%
echo .  make guides: %DIFF_MAKEGUIDES%       >> %infofile%
echo .        total: %DIFF_TOTALTIME%        >> %infofile%
echo .------------------------------         >> %infofile%

copy %infofile% %timingslogfile%

:: email results

sed "s/$/\r/" < %warninglog% > %warninglogpc%
sed "s/$/\r/" < %errorlog% > %errorlogpc%

if %havewarnings% == 0 (
  if %haveerrors% == 0 (
    set message=success
  ) else (
    echo. >> %infofile%
    type %errorlogpc% >> %infofile%
    set message=failure
  )
) else (
  if %haveerrors% == 0 (
    echo. >> %infofile%
    type %warninglogpc% >> %infofile%
    set message=success with warnings
  ) else (
    echo. >> %infofile%
    type %errorlogpc% >> %infofile%
    echo. >> %infofile%
    type %warninglogpc% >> %infofile%
    set message=failure
  )
)

if exist %emailexe% (
  call %email% %mailToCFAST% "cfastbot %message% on %COMPUTERNAME%! %revisionstring%" %infofile%
)

echo cfastbot %message% on %COMPUTERNAME%! %revisionstring%, runtime: %DIFF_TOTALTIME%
echo Results summarized in %infofile%.
cd %CURDIR%
goto eof

::------------ cfastbot "subroutines below ----------------------

:: -------------------------------------------------------------
:output_abort_message
:: -------------------------------------------------------------
  echo "***Fatal error: cfastbot build failure on %COMPUTERNAME% %revision%"
exit /b

:: -------------------------------------------------------------
:WAIT_RUN
:: -------------------------------------------------------------

set prog=%1
:loop5
tasklist | grep -ic %prog% > temp.out
set /p numexe=<temp.out
if %numexe% == 0 goto finished5
Timeout /t 30 >nul 
goto loop5

:finished5
exit /b

:: -------------------------------------------------------------
:GET_DURATION
:: -------------------------------------------------------------

:: compute difftime=time2 - time1

set label=%1
set time1=%2

set difftime=DIFF_%label%
call :GET_TIME time2

set /a diff=%time2% - %time1%
set /a diff_h= %diff%/3600
set /a diff_m= (%diff% %% 3600 )/60
set /a diff_s= %diff% %% 60
if %diff% GEQ 3600 set duration= %diff_h%h %diff_m%m %diff_s%s
if %diff% LSS 3600 if %diff% GEQ 60 set duration= %diff_m%m %diff_s%s
if %diff% LSS 3600 if %diff% LSS 60 set duration= %diff_s%s
echo %label%: %duration% >> %stagestatus%
set %difftime%=%duration%
exit /b 0

:: -------------------------------------------------------------
:GET_TIME
:: -------------------------------------------------------------

set arg1=%1

%gettimeexe% > %timefile%
set /p %arg1%=<%timefile%
exit /b 0

:: -------------------------------------------------------------
:is_file_installed
:: -------------------------------------------------------------

  set program=%1
  %program% --help 1>> %OUTDIR%\stage_exist.txt 2>&1
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
  :find_runcases_warnings
:: -------------------------------------------------------------

set search_string=%1
set search_dir=%2
set stage=%3

cd %search_dir%
grep -RIiE %search_string% --include *.log --include *.out --include *.err * > %OUTDIR%\stage_warning.txt
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
  :find_runcases_errors
:: -------------------------------------------------------------

set search_string=%1
set search_dir=%2
set stage=%3

cd %search_dir%
grep -RIiE %search_string% --include *.log --include *.out --include *.err * > %OUTDIR%\stage_error.txt
type %OUTDIR%\stage_error.txt | find /v /c "kdkwokwdokwd"> %OUTDIR%\stage_nerror.txt
set /p nerrors=<%OUTDIR%\stage_nerror.txt
if %nerrors% GTR 0 (
  echo %stage% errors >> %errorlog%
  echo. >> %errorlog%
  type %OUTDIR%\stage_error.txt >> %errorlog%
  set haveerrors=1
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
 :git_clean
:: -------------------------------------------------------------

set gitcleandir=%1
cd %gitcleandir%
git clean -dxf  1> Nul 2>&1
git add . 1> Nul 2>&1
git reset --hard HEAD 1> Nul 2>&1
exit /b

:: -------------------------------------------------------------
 :build_guide
:: -------------------------------------------------------------

set guide=%1
set guide_dir=%2

set guideout=%OUTDIR%\stage6_%guide%.txt

cd %guide_dir%

git describe --long --dirty > gitinfo.txt
set /p gitrevision=<gitinfo.txt
echo \newcommand^{\gitrevision^}^{%gitrevision%^} > ..\Bibliography\gitrevision.tex

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

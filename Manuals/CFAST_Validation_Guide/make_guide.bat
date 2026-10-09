@echo off
set paper=CFAST_Validation_Guide

git describe --long --dirty > gitinfo.txt
set /p gitrevision=<gitinfo.txt
echo \newcommand^{\gitrevision^}^{%gitrevision%^} > ..\Bibliography\gitrevision.tex

pdflatex -interaction nonstopmode %paper% > %paper%.err
if errorlevel 1 goto failed
biber %paper% > %paper%.err
if errorlevel 1 goto failed
set pass=0
:reference_pass
set /a pass+=1
call :reference_snapshot "%paper%.refs-before"
echo Building %paper%: reference pass %pass%
pdflatex -interaction nonstopmode %paper% > %paper%.err
if errorlevel 1 goto failed
call :reference_snapshot "%paper%.refs-after"
fc /b "%paper%.refs-before" "%paper%.refs-after" > nul
if not errorlevel 1 goto references_stable
if %pass% LSS 6 goto reference_pass
echo %paper% references did not stabilize after six passes
goto failed

:references_stable
findstr /c:"! LaTeX Error:" /c:"Fatal error" /c:"Error:" /c:"undefined" /c:"multiply defined" /c:"multiply-defined" %paper%.err
if not errorlevel 1 goto failed
call :cleanup
echo %paper% build complete
exit /b 0

:reference_snapshot
type nul > "%~1"
for %%F in (*.aux %paper%.toc %paper%.lof %paper%.lot) do type "%%F" >> "%~1"
exit /b 0

:cleanup
if exist %paper%.refs-before del %paper%.refs-before
if exist %paper%.refs-after del %paper%.refs-after
if exist ..\Bibliography\gitrevision.tex erase ..\Bibliography\gitrevision.tex
exit /b 0

:failed
call :cleanup
echo %paper% build failed
exit /b 1

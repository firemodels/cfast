@echo off
set paper=CFAST_Users_Guide

git describe --long --dirty > gitinfo.txt
set /p gitrevision=<gitinfo.txt
echo \newcommand^{\gitrevision^}^{%gitrevision%^} > ..\Bibliography\gitrevision.tex

pdflatex -interaction nonstopmode %paper% > %paper%.err
bibtex %paper% > %paper%.err
pdflatex -interaction nonstopmode %paper% > %paper%.err
pdflatex -interaction nonstopmode %paper% > %paper%.err

find "! LaTeX Error:" %paper%.err
find "Fatal error" %paper%.err
find "Error:" %paper%.err

if exist ..\Bibliography\gitrevision.tex erase ..\Bibliography\gitrevision.tex
echo %paper% build complete



echo off
echo Removing old and temporary files.  Messages can be ignored here.
del *_n.csv /s/q >nul
del *_s.csv /s/q >nul
del *_f.csv /s/q >nul
del *_w.csv /s/q >nul
del *_zone.csv /s/q >nul
del *.out /s/q >nul
del *.smv /s/q >nul
del *.plt /s/q >nul
del *.status /s/q >nul
del *.log /s/q >nul
del ..\Docs\Validation_Guide\FIGURES\Verification\*.pdf /q >nul
echo Done removing old and temporary files.
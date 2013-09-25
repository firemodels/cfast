@echo off
cd ..
echo.
echo dassl errors
findstr /D:".." /S "dassl" *.log
echo.
echo snsq errors
findstr /D:".." /S "snsq" *.log

Rem findstr /D:".." /S "execution" *.log
cd scripts
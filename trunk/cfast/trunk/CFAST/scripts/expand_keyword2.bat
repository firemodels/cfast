@echo off
set keyword=%1
set newvalue=%2
set newvalue2=%3
set file=%4

if NOT exist %file% (
  exit 1 /b
)
sed -e "s/$%keyword%.*\$/$%keyword% %newvalue% %newvalue2% $/g" %file% | sed "s/$/\r/" > %temp%\temp.txt
copy %temp%\temp.txt %file% 1> Nul 2>&1

@echo off
set keyword=%1
set newvalue=%2
set file=%3

if NOT exist %file% (
  exit 1 /b
)
sed -e "s/$%keyword%.*\$/$%keyword% %newvalue% $/g" %file% | sed "s/$/\r/" > %temp%\temp.txt
copy %temp%\temp.txt %file%

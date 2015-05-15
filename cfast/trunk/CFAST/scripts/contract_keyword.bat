@echo off
set keyword=%1
set file=%2

if NOT exist %file% (
  exit 1 /b
)

sed -e "s/$%keyword%.*\$/$%keyword%: unknown $/g" %file% | sed "s/$/\r/" > %temp%\temp2.txt
copy %temp%\temp2.txt %file% 1> Nul 2>&1
goto eof

:is_fds_installed
echo | call fds 1> %temp%\file_exist.txt 2>&1
type %temp%\file_exist.txt | find /i /c "not recognized" > %temp%\file_exist_count.txt
set /p nothave=<%temp%\file_exist_count.txt
set fdsinstalled=1
if %nothave% == 1 (
  set fdsinstalled=0
)
exit /b 0

:eof

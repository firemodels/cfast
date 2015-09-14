@echo off
set smvdir=%userprofile%\FIRE-LOCAL\software\smokeview
set bindir=..\bin

copy  %smvdir%\objects.svo        %bindir%\
copy  %smvdir%\pthreadVC2_x64.dll %bindir%\
copy  %smvdir%\smokediff.exe      %bindir%\
copy  %smvdir%\smokeview.exe      %bindir%\

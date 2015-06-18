@echo off
set CURDIR=%CD%
set repo=cfastgitclean

cd %userprofile%\%repo%
git pull
cd Utilities\cfastbot
copy *.bat %CURDIR%
cd %CURDIR%

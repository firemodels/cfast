@echo off
set CURDIR=%CD%
set repo=%1

cd %userprofile%\%repo%
git pull
cd %CURDIR%
copy %userprofile%\%repo%\Utilities\cfastbot\*.bat %CURDIR%

@echo off
set CURDIR=%CD%
set repo=cfastgitclean

cd %userprofile%\%repo%
git pull
cd %CURDIR%
copy %userprofile%\%repo%\Utilities\Firebot\*.bat %CURDIR%

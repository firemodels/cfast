:: @echo off

set CURDIR=%CD%

:: define cfast_root

cd ..\..\..
set cfast_root=%CD%

@echo *** Create CFAST executables

call %cfast_root%\Build\scripts\setup_intel_compilers.bat intel64

cd %cfast_root%\Build\CFAST\intel_win_64
call make_cfast.bat bot release
copy cfast7_win_64.exe %cfast_root%\Utilities\for_bundle\Bin\cfast.exe /Y
cd %cfast_root%\Build\Cedit
call make_cedit.bat bot
cd %cfast_root%\Utilities\for_bundle\scripts

copy %cfast_root%\..\Extras\Bin\*.* %cfast_root%\Utilities\for_bundle\Bin\ /Y

@echo *** Copying Smokeview executables

if NOT exist %cfast_root%\Utilities\for_bundle\SMV6 (
   mkdir %cfast_root%\Utilities\for_bundle\SMV6
)
copy %cfast_root%\..\Extras\SMV6\*.* %cfast_root%\Utilities\for_bundle\SMV6\ /Y
if NOT exist %cfast_root%\Utilities\for_bundle\SMV6\textures (
   mkdir %cfast_root%\Utilities\for_bundle\SMV6\textures
)
copy %cfast_root%\..\Extras\SMV6\textures\*.* %cfast_root%\Utilities\for_bundle\SMV6\textures\ /Y

@echo *** copy install utilities
copy %cfast_root%\..\..\bin\set_path.exe %cfast_root%\Utilities\for_bundle\bin\ /Y
copy %cfast_root%\..\..\bin\Shortcut.exe %cfast_root%\Utilities\for_bundle\bin\ /Y
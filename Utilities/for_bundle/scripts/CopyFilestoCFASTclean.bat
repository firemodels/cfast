:: @echo off

set cfastroot=c:\Users\rpeacoc\firemodels\cfast

@echo *** Create CFAST executables

call %cfastroot%\Build\scripts\setup_intel_compilers.bat intel64

cd %cfastroot%\Build\CFAST\intel_win_64
call make_cfast.bat bot release
copy cfast7_win_64.exe %cfastroot%\Utilities\for_bundle\Bin\cfast.exe /Y
cd %cfastroot%\Build\Cedit
call make_cedit.bat bot
cd %cfastroot%\Utilities\for_bundle\scripts

copy %cfastroot%\..\Extras\Bin\*.* %cfastroot%\Utilities\for_bundle\Bin\ /Y

@echo *** Copying Smokeview executables

if NOT exist %cfastroot%\Utilities\for_bundle\SMV6 (
   mkdir %cfastroot%\Utilities\for_bundle\SMV6
)
copy %cfastroot%\..\Extras\SMV6\*.* %cfastroot%\Utilities\for_bundle\SMV6\ /Y
if NOT exist %cfastroot%\Utilities\for_bundle\SMV6\textures (
   mkdir %cfastroot%\Utilities\for_bundle\SMV6\textures
)
copy %cfastroot%\..\Extras\SMV6\textures\*.* %cfastroot%\Utilities\for_bundle\SMV6\textures\ /Y

@echo *** copy install utilities
copy %cfastroot%\..\..\bin\set_path.exe %cfastroot%\Utilities\for_bundle\scripts\bundleinfo\ /Y
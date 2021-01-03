:: @echo off

set CURDIR=%CD%

:: define cfast_root

cd ..\..\..
set cfast_root=%CD%

@echo ***     Create NPlot
cd %cfast_root%\..\Extras\nplot
set MSBUILD="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin\MSBuild.exe"
%MSBUILD%  NPlot.sln /target:NPlot /p:Configuration=Release /p:Platform="Any CPU"
copy %cfast_root%\..\Extras\nplot\src\bin\NPlot.dll %cfast_root%\Utilities\for_bundle\Bin\ /Y

@echo *** Create CFAST executables

@echo ***     Create CEdit
cd %cfast_root%\Build\Cedit
call make_cedit.bat bot
copy %cfast_root%\Source\Cedit\obj\Release\CEdit.exe %cfast_root%\Utilities\for_bundle\Bin\cEdit.exe /Y

@echo ***     Create CFAST
call %cfast_root%\Build\scripts\setup_intel_compilers.bat intel64
cd %cfast_root%\Build\CFAST\intel_win_64
call make_cfast.bat bot release
copy cfast7_win_64.exe %cfast_root%\Utilities\for_bundle\Bin\cfast.exe /Y

@echo ***     Create CData
call %cfast_root%\Build\scripts\setup_intel_compilers.bat intel64
cd %cfast_root%\Build\Cdata\intel_win_64
call make_cdata.bat bot release
copy cdata7_win_64.exe %cfast_root%\Utilities\for_bundle\Bin\cdata.exe /Y

@echo ***     Create VandVCalcs
cd %cfast_root%\Build\VandV_Calcs\intel_win_64
call make_vv.bat bot release
copy VandV_Calcs_win_64.exe %cfast_root%\Utilities\for_bundle\Bin\VandV_Calcs.exe /Y

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
copy %cfast_root%\..\Extras\SMV6\set_path.exe %cfast_root%\Utilities\for_bundle\bin\ /Y
copy %cfast_root%\..\Extras\Bin\Shortcut.exe %cfast_root%\Utilities\for_bundle\bin\ /Y
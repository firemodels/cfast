@echo off

:: setup compiler environment
cd ..\..\Source\CEdit
if exist temp rmdir /s /q temp
mkdir temp

set KWDIR=..\..\Utilities\keyword
set SDIR=.\
set BINDIR=..\..\Utilities\for_bundle\Bin
set MSBUILD="C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin\MSBuild.exe"

Title Building CEdit for 64 bit Windows

@echo Add repository info to CEdit source
call %KWDIR%\expand_file.bat %SDIR% %SDIR%About.vb

@echo Compile code
cd ..\..
%MSBUILD% CFAST.sln /target:CEdit /p:Configuration=Release

@echo Remove repository info from CEdit source
cd Source\CEdit
call %KWDIR%\contract_file.bat %SDIR%\About.vb

if x%arg1% == xbot goto skip2
pause
:skip2
cd ..\..\Build\Cedit


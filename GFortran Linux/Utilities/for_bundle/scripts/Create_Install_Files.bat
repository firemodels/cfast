@echo off

set bindir=%cfast_root%\Utilities\for_bundle\Bin
set vandvdir=%cfast_root%\Build\VandV_Calcs\intel_win_64
set docdir=%cfast_root%\Manuals
set CURDIR2=%CD%

echo.
echo ***making directories
echo.

if exist %DISTDIR% rmdir /s /q %DISTDIR%
mkdir %DISTDIR%
mkdir %DISTDIR%\Examples
mkdir %DISTDIR%\Documents
mkdir %DISTDIR%\Uninstall

set SMVDISTDIR=%DISTDIR%\..\SMV6
if exist %SMVDISTDIR% rmdir /s /q %SMVDISTDIR%
mkdir %SMVDISTDIR%
mkdir %SMVDISTDIR%\textures

echo.
echo ***Copying executables
echo.

call :COPY  %bindir%\CData.exe %DISTDIR%\
call :COPY  %bindir%\CEdit.exe %DISTDIR%\
call :COPY  %bindir%\CFAST.exe %DISTDIR%\
call :COPY  %vandvdir%\VandV_Calcs_win_64.exe %DISTDIR%\VandV_Calcs.exe
call :COPY  %bindir%\background.exe %DISTDIR%\

echo.
echo ***Copying CFAST DLLs
echo.

call :COPY  %bindir%\C1*.dll   %DISTDIR%\
call :COPY  %bindir%\NPlot.dll %DISTDIR%\

echo.
echo ***Copying CFAST support files
echo.

call :COPY  %bindir%\AllFires.in              %DISTDIR%\
call :COPY  %bindir%\thermal.csv              %DISTDIR%\

echo.
echo ***Copying CFAST example files
echo.

call :COPY  %bindir%\Data\Users_Guide_Example.in %DISTDIR%\Examples\
call :COPY  %docdir%\CData_Guide\Examples\*.in   %DISTDIR%\Examples\

echo.
echo ***Copying CFAST documentation
echo.

call :COPY %docdir%\Tech_Ref\Tech_Ref.pdf                       %DISTDIR%\Documents\
call :COPY %docdir%\Users_Guide\Users_Guide.pdf                 %DISTDIR%\Documents\
call :COPY %docdir%\Validation_Guide\Validation_Guide.pdf       %DISTDIR%\Documents\
call :COPY %docdir%\Configuration_Guide\Configuration_Guide.pdf %DISTDIR%\Documents\
call :COPY %docdir%\CData_Guide\CData_Guide.pdf                 %DISTDIR%\Documents\

echo.
echo ***Copying Smokeview files
echo.

call :COPY %bindir%\..\SMV6\background.exe			%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\get_time.exe			%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\sh2bat.exe				%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\smokediff.exe			%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\smokeview.exe			%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\smokezip.exe			%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\wind2fds.exe			%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\objects.svo				%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\textures				%SMVDISTDIR%\
call :COPY %bindir%\..\SMV6\volrender.ssf			%SMVDISTDIR%\
copy %bindir%\..\SMV6\textures\*.jpg				%SMVDISTDIR%\textures\
copy %bindir%\..\SMV6\textures\*.png				%SMVDISTDIR%\textures\

echo.
echo ***Copying Uninstall files
echo.

call :COPY  %bundleinfo%\uninstall.bat        %DISTDIR%\Uninstall
call :COPY  %bundleinfo%\uninstall_cfast.bat  %DISTDIR%\Uninstall\uninstall_base.bat 
call :COPY  %bundleinfo%\uninstall_cfast2.bat %DISTDIR%\Uninstall\uninstall_base2.bat 
call :COPY  %bundleinfo%\uninstall_cfast2.bat %DISTDIR%\Uninstall\uninstall_base2.bat

call :COPY  %bindir%\set_path.exe %DISTDIR%\set_path.exe
call :COPY  %bindir%\Shortcut.exe %DISTDIR%\Shortcut.exe

cd %CURDIR%

GOTO :EOF

:COPY
set label=%~n1%~x1
set infiletime=%~t1
set infile=%1
set outfile=%2
IF EXIST %infile% (
   echo Copying %label%, %infiletime%
   copy %infile% %outfile%
) ELSE (
   echo.
   echo *** warning: %infile% does not exist
   echo.
   pause
)
exit /b

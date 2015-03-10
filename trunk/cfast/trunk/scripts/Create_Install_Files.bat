
set bindir=%svn_root%\bin
set docdir=%svn_root%\Docs

echo.
echo ***making directories
echo.

if exist %DISTDIR% rmdir /s /q %DISTDIR%
mkdir %DISTDIR%
mkdir %DISTDIR%\Examples
mkdir %DISTDIR%\Documents

echo.
echo ***Copying executables
echo.

call :COPY  %bindir%\CEdit.exe %DISTDIR%\
call :COPY  %bindir%\CFAST.exe %DISTDIR%\

echo.
echo ***Copying CFAST DLLs
echo.

call :COPY  %bindir%\C1.C1Report.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.C1Zip.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.Win.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.Win.C1Document.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.Win.C1FlexGrid.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.Win.C1Report.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.Win.C1ReportDesigner.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.Win.C1Sizer.4.dll %DISTDIR%\
call :COPY  %bindir%\C1.Win.C1Ssrs.4.dll %DISTDIR%\
call :COPY  %bindir%\NPlot.dll %DISTDIR%\


echo.
echo ***Copying CFAST support files
echo.

call :COPY  %bindir%\3_panel_workstation.o %DISTDIR%\
call :COPY  %bindir%\bunkbed.o %DISTDIR%\
call :COPY  %bindir%\burner.o %DISTDIR%\
call :COPY  %bindir%\curtains.o %DISTDIR%\
call :COPY  %bindir%\kiosk.o %DISTDIR%\
call :COPY  %bindir%\mattress_and_boxspring.o %DISTDIR%\
call :COPY  %bindir%\sofa.o %DISTDIR%\
call :COPY  %bindir%\tv_set.o %DISTDIR%\
CALL :COPY  %bindir%\ upholstered_chair.o %DISTDIR%\
call :COPY  %bindir%\wardrobe.o %DISTDIR%\
call :COPY  %bindir%\wood_wall.o %DISTDIR%\
call :COPY  %bindir%\thermal.csv %DISTDIR%\

echo.
echo ***Copying CFAST example files
echo.

call :COPY  %bindir%\Data\standard.in %DISTDIR%\Examples\

echo.
echo ***Copying CFAST documentation
echo.

call :COPY %docdir%\Tech_Ref\Tech_Ref.pdf %DISTDIR%\Documents\
call :COPY %docdir%\Users_Guide\Users_Guide.pdf %DISTDIR%\Documents\
call :COPY %docdir%\Validation_Guide\Validation_Guide.pdf %DISTDIR%\Documents\

echo.
echo ***Copying Smokeview files
echo.

call :COPY  %bindir%\objects.svo %DISTDIR%\
call :COPY  %bindir%\pthreadVC2_x64.dll %DISTDIR%\
call :COPY  %bindir%\smokediff.exe %DISTDIR%\
call :COPY  %bindir%\smokeview.exe %DISTDIR%\



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

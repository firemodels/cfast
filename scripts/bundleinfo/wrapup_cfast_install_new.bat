@echo off

echo.
echo *** Wrapping up the cfast installation.
echo.

set CFASTBINDIR=%CD%
cd ..\SMV6
set SMVBINDIR=%CD%
cd %CFASTBINDIR%

:: ------------ setting up path ------------

echo.
echo *** Setting up the PATH variables.

echo.
echo *** Removing previous CFAST entries from the system and user path.
call "%CFASTBINDIR%\set_path.exe" -s -m -b -r "cfast6" >Nul
call "%CFASTBINDIR%\set_path.exe" -s -m -b -r "cfast7" >Nul
call "%CFASTBINDIR%\set_path.exe" -s -m -b -r "firemodels\FDS6" >Nul
call "%CFASTBINDIR%\set_path.exe" -s -m -b -r "firemodels\SMV6" >Nul


call "%CFASTBINDIR%\set_path.exe" -s -m -f "%CFASTBINDIR%" >Nul
call "%CFASTBINDIR%\set_path.exe" -s -m -f "%SMVBINDIR%" >Nul

:: ------------- file association -------------
echo.
echo *** Associating the .in file extension with CEdit.exe

ftype ceditDoc="%CFASTBINDIR%\CEdit.exe" %%1 >Nul
assoc .in=ceditDoc>Nul

echo.
echo *** Associating the .smv file extension with smokeview.exe
ftype smvDoc="%SMVBINDIR%\smokeview.exe" "%%1" >Nul
assoc .smv=smvDoc>Nul

:: ------------- start menu shortcuts ---------------

echo. 
echo *** Adding document shortcuts to the Start menu.

set cfaststartmenu=%ALLUSERSPROFILE%\Start Menu\Programs\cfast
if exist "%cfaststartmenu%" rmdir /q /s "%cfaststartmenu%" >Nul

mkdir "%cfaststartmenu%"

mkdir "%cfaststartmenu%\Guides"
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Users Guide.lnk"                                     /T:"%CFASTBINDIR%\Documents\Users_Guide.pdf"         /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Technical Reference Guide.lnk"                       /T:"%CFASTBINDIR%\Documents\Tech_Ref.pdf"            /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Software Development and Model Evaluation Guide.lnk" /T:"%CFASTBINDIR%\Documents\Validation_Guide.pdf"    /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Configuration Management.lnk"                        /T:"%CFASTBINDIR%\Documents\Configuration_Guide.pdf" /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\CEdit.lnk"                                                        /T:"%CFASTBINDIR%\CEdit.exe"                         /A:C >NUL

erase "%CFASTBINDIR%"\set_path.exe
erase "%CFASTBINDIR%"\shortcut.exe

echo.
echo *** Press any key to complete the installation.
pause>NUL



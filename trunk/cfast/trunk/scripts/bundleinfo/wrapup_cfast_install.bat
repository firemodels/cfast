@echo off

echo.
echo *** Wrapping up the cfast installation.
echo.

:: ------------ setting up path ------------

echo.
echo *** Setting up the PATH variable.

call "%CD%\set_path.exe" -s -m -f "%CD%" >Nul

:: ------------- file association -------------
echo.
echo *** Associating the .in file extension with CEdit.exe

ftype ceditDoc="%CD%\CEdit.exe" %%1 >Nul
assoc .in=ceditDoc>Nul

:: ftype smvDoc="%CD%\bin\smokeview.exe" "%%1" >Nul
:: assoc .smv=smvDoc>Nul

:: ------------- start menu shortcuts ---------------

echo. 
echo *** Adding document shortcuts to the Start menu.

set cfaststartmenu=%ALLUSERSPROFILE%\Start Menu\Programs\cfast
if exist "%cfaststartmenu%" rmdir /q /s "%cfaststartmenu%" >Nul

mkdir "%cfaststartmenu%"

mkdir "%cfaststartmenu%\Guides"
"%CD%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Users Guide.lnk"       /T:"%CD%\Documents\Users_Guide.pdf" /A:C >NUL
"%CD%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Technical Reference Guide.lnk"       /T:"%CD%\Documents\Tech_Ref.pdf" /A:C >NUL
"%CD%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Software Development and Model Evaluation Guide.lnk"       /T:"%CD%\Documents\Validation_Guide.pdf" /A:C >NUL
"%CD%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Configuration Management.lnk"       /T:"%CD%\Documents\Configuration_Guide.pdf" /A:C >NUL
"%CD%\shortcut.exe" /F:"%cfaststartmenu%\CEdit.lnk"       /T:"%CD%\CEdit.exe" /A:C >NUL

erase "%CD%"\set_path.exe
erase "%CD%"\shortcut.exe

echo.
echo *** Press any key to complete the installation.
pause>NUL



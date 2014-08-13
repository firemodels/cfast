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

ftype inCEdit="%CD%\CEdit.exe" "%%1" >Nul
assoc .in=inCEdit>Nul

:: ftype smvDoc="%CD%\bin\smokeview.exe" "%%1" >Nul
:: assoc .smv=smvDoc>Nul

:: ------------- start menu shortcuts ---------------

echo. 
echo *** Adding document shortcuts to the Start menu.

set cfaststartmenu=%ALLUSERSPROFILE%\Start Menu\Programs\cfast
if exist "%cfaststartmenu%" rmdir /q /s "%cfaststartmenu%"

mkdir "%cfaststartmenu%"

mkdir "%cfaststartmenu%\Guides and Release Notes"
:: the following line is a placeholder - need to update with actual documents
"%CD%\shortcut.exe" /F:"%cfaststartmenu%\Guides and Release Notes\CFAST Users Guide.lnk"       /T:"%CD%\Documentation\CFAST_Users_Guide.pdf" /A:C >NUL
"%CD%\shortcut.exe" /F:"%cfaststartmenu%\CEdit.lnk"       /T:"%CD%\CEdit.exe" /A:C >NUL

erase "%CD%"\set_path.exe
erase "%CD%"\shortcut.exe

echo.
echo *** Press any key to complete the installation.
pause>NUL



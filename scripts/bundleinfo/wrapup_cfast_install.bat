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

set cfaststartmenu=%ALLUSERSPROFILE%\Start Menu\Programs\CFAST7
if exist "%cfaststartmenu%" rmdir /q /s "%cfaststartmenu%" >Nul

mkdir "%cfaststartmenu%"

mkdir "%cfaststartmenu%\Guides"
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Users Guide.lnk"                                     /T:"%CFASTBINDIR%\Documents\Users_Guide.pdf"         /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Technical Reference Guide.lnk"                       /T:"%CFASTBINDIR%\Documents\Tech_Ref.pdf"            /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Software Development and Model Evaluation Guide.lnk" /T:"%CFASTBINDIR%\Documents\Validation_Guide.pdf"    /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Guides\CFAST Configuration Management.lnk"                        /T:"%CFASTBINDIR%\Documents\Configuration_Guide.pdf" /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\CFAST7.lnk"                                                       /T:"%CFASTBINDIR%\CEdit.exe"                         /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Smokeview.lnk"                                                    /T:"%SMVBINDIR%\smokeview.exe"                       /A:C >NUL
"%CFASTBINDIR%\shortcut.exe" /F:"%cfaststartmenu%\Uninstall.lnk"                                                    /T:"%CFASTBINDIR%\Uninstall\uninstall.bat"           /A:C >NUL

:: ----------- setting up uninstall file

echo.
echo *** Setting up Uninstall script.
echo echo.                                                                >> Uninstall\uninstall_base.bat

:: remove smokeview path and directory
echo if %%fdsinstalled%% == 1 goto skip2                                  >> Uninstall\uninstall_base.bat
echo echo Removing directory, %SMVBINDIR%, from the System Path           >> Uninstall\uninstall_base.bat
echo call "%CFASTBINDIR%\Uninstall\set_path.exe" -s -b -r "%SMVBINDIR%"   >> Uninstall\uninstall_base.bat
echo rmdir /s /q "%SMVBINDIR%"                                            >> Uninstall\Uninstall_base.bat
echo :skip2                                                               >> Uninstall\uninstall_base.bat

:: remove cfast path and directory
echo echo Removing directory, %CFASTBINDIR% , from the System Path        >> Uninstall\uninstall_base.bat
echo call "%CFASTBINDIR%\Uninstall\set_path.exe" -s -b -r "%CFASTBINDIR%" >> Uninstall\uninstall_base.bat
echo echo.                                                                >> Uninstall\uninstall_base.bat
echo echo Removing %CFASTBINDIR%                                          >> Uninstall\uninstall_base.bat
echo rmdir /s /q "%CFASTBINDIR%"                                          >> Uninstall\Uninstall_base.bat
echo pause                                                                >> Uninstall\Uninstall_base.bat

echo echo *** Uninstall complete                                          >> Uninstall\uninstall_base.bat
echo pause>Nul                                                            >> Uninstall\uninstall_base.bat

type Uninstall\uninstall_base2.bat                                        >> Uninstall\uninstall_base.bat
erase Uninstall\uninstall_base2.bat

echo "%CFASTBINDIR%\Uninstall\uninstall.vbs"                              >> Uninstall\uninstall.bat
echo echo Uninstall complete                                              >> Uninstall\uninstall.bat
echo pause                                                                >> Uninstall\uninstall.bat

set ELEVATE_APP=%CFASTBINDIR%\Uninstall\Uninstall_base.bat
set ELEVATE_PARMS=
echo Set objShell = CreateObject("Shell.Application")                       > Uninstall\uninstall.vbs
echo Set objWshShell = WScript.CreateObject("WScript.Shell")               >> Uninstall\uninstall.vbs
echo Set objWshProcessEnv = objWshShell.Environment("PROCESS")             >> Uninstall\uninstall.vbs
echo objShell.ShellExecute "%ELEVATE_APP%", "%ELEVATE_PARMS%", "", "runas" >> Uninstall\uninstall.vbs

erase "%CFASTBINDIR%"\set_path.exe
erase "%CFASTBINDIR%"\shortcut.exe

echo.
echo *** Press any key to complete the installation.
pause>NUL



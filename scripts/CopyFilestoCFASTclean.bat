@echo off

set cfastroot=c:\Users\rpeacoc\cfastgitclean

@echo *** Copying CFAST Executables
copy ..\Bin\CFAST.exe %cfastroot%\bin /Y
copy ..\Bin\CEdit.exe %cfastroot%\bin /Y

@echo *** Copying Smokeview executables

if NOT exist %cfastroot%\SMV6 (
   mkdir %cfastroot%\SMV6
)
copy ..\SMV6\*.* %cfastroot%\SMV6\ /Y
if NOT exist %cfastroot%\SMV6\textures (
   mkdir %cfastroot%\SMV6\textures
)
copy ..\SMV6\textures\*.* %cfastroot%\SMV6\textures\ /Y

@echo *** copying DLLs
copy ..\bin\C1*.dll %cfastroot%\bin /Y
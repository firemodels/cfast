:: @echo off

set cfastroot=c:\Users\rpeacoc\cfastgitclean

@echo *** Copying CFAST Executables
copy ..\Bin\CFAST.exe %cfastroot%\Utilities\for_bundle\Bin /Y
copy ..\Bin\CEdit.exe %cfastroot%\Utilities\for_bundle\Bin /Y

@echo *** Copying Smokeview executables

if NOT exist %cfastroot%\Utilities\for_bundle\SMV6 (
   mkdir %cfastroot%\Utilities\for_bundle\SMV6
)
copy ..\SMV6\*.* %cfastroot%\Utilities\for_bundle\SMV6\ /Y
if NOT exist %cfastroot%\Utilities\for_bundle\SMV6\textures (
   mkdir %cfastroot%\Utilities\for_bundle\SMV6\textures
)
copy ..\SMV6\textures\*.* %cfastroot%\Utilities\for_bundle\SMV6\textures\ /Y

@echo *** copying DLLs
copy ..\Bin\C1*.dll %cfastroot%\Utilities\for_bundle\Bin /Y

@echo *** copying additional files
copy ..\Bin\*.o %cfastroot%\Utilities\for_bundle\Bin /Y

@echo *** copy install utilities
copy \users\rpeacoc\FIRE-LOCAL\repo_exes\set_path.exe %cfastroot%\Utilities\for_bundle\scripts\bundleinfo\ /Y
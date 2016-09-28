:: @echo off

set cfastroot=c:\Users\rpeacoc\firemodels\cfast

@echo *** Copying Smokeview executables

if NOT exist %cfastroot%\Utilities\for_bundle\SMV6 (
   mkdir %cfastroot%\Utilities\for_bundle\SMV6
)
copy %cfastroot%\..\Extras\SMV6\*.* %cfastroot%\Utilities\for_bundle\SMV6\ /Y
if NOT exist %cfastroot%\Utilities\for_bundle\SMV6\textures (
   mkdir %cfastroot%\Utilities\for_bundle\SMV6\textures
)
copy %cfastroot%\..\Extras\SMV6\textures\*.* %cfastroot%\Utilities\for_bundle\SMV6\textures\ /Y

@echo *** copy install utilities
copy \users\rpeacoc\FIRE-LOCAL\repo_exes\set_path.exe %cfastroot%\Utilities\for_bundle\scripts\bundleinfo\ /Y
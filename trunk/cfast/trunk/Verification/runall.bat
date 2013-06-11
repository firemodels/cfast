echo off
echo.| time
echo Running CFAST simulations. $Rev: 846 $
if "%1"=="" goto Help
if %1==ALL goto All
if %1==Mass goto Mass

:Help
echo Choose ALL, Mass
goto end
:ALL
call cleanall.bat
:Mass
echo Running Mass Balance Tests
cd Mass_Energy_Balance
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\..\bin\cfast Base /V
cd ..
if %1==Mass goto end

:end
echo.| time
echo CFAST simulations complete.

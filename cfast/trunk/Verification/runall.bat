echo off
echo.| time
echo Running CFAST simulations. $Rev: 846 $
if "%1"=="" goto Help
if %1==ALL goto All
if %1==Base goto Base
if %1==Analytical goto Analytical
if %1==NoFire goto NoFire
if %1==Fire goto Fire
if %1==DOE goto DOE
:Help
echo Choose ALL, Analytical, Fire, DOE
goto end
:ALL
call cleanall.bat

:Base
echo Running Base Case Tests
cd Analytical
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast Base /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib_wallsoff /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib_window /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib_window_elevation /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib_window_geometry /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib_window_wind /V
cd ..
if %1==Base goto end

:Analytical
echo Running Basic Mass Balance Tests
cd Analytical\Mass
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\..\..\bin\cfast basic_mechvent /V
cd ..\..
if %1==Analytical goto end

:NoFire
echo Running Basic Energy Balance Tests
cd Mass_Energy_Balance
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast basic_pressure /V
..\scripts\background -u 98 ..\..\bin\cfast basic_pressure_vent /V
..\scripts\background -u 98 ..\..\bin\cfast basic_pressure_wallsoff /V
..\scripts\background -u 98 ..\..\bin\cfast basic_mechvent_dropoff /V
..\scripts\background -u 98 ..\..\bin\cfast basic_connection_floorceiling_mechvent /V
cd ..
if %1==NoFire goto end

:Fire
echo Running Fire Tests
cd Mass_Energy_Balance
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\..\bin\cfast 100kW_fire /V
..\scripts\background -u 98 ..\..\bin\cfast fire /V
..\scripts\background -u 98 ..\..\bin\cfast fire_window /V
..\scripts\background -u 98 ..\..\bin\cfast fire_window_windowchange /V
..\scripts\background -u 98 ..\..\bin\cfast fire_window_geochange /V
..\scripts\background -u 98 ..\..\bin\cfast fire_ceiling /V
..\scripts\background -u 98 ..\..\bin\cfast fire_ceiling_ventsize /V
..\scripts\background -u 98 ..\..\bin\cfast fire_ceiling_geochange /V
..\scripts\background -u 98 ..\..\bin\cfast fire_mechanical_vent /V
..\scripts\background -u 98 ..\..\bin\cfast fire_HRRdoubled /V
..\scripts\background -u 98 ..\..\bin\cfast fire_HRRarea2 /V
..\scripts\background -u 98 ..\..\bin\cfast fire_sprinkler /V
..\scripts\background -u 98 ..\..\bin\cfast fire_sprinkler_density /V
..\scripts\background -u 98 ..\..\bin\cfast fire_sprinkler_RTI /V
..\scripts\background -u 98 ..\..\bin\cfast fire_sprinkler_HRRdoubled /V
..\scripts\background -u 98 ..\..\bin\cfast fire_hexane /V
..\scripts\background -u 98 ..\..\bin\cfast fire_urethane /V
..\scripts\background -u 98 ..\..\bin\cfast fire_hardwood /V
..\scripts\background -u 98 ..\..\bin\cfast fire_ignitiontemp_120 /V
..\scripts\background -u 98 ..\..\bin\cfast fire_ignitiontemp_700 /V
..\scripts\background -u 98 ..\..\bin\cfast fire_filterefficiency /V
..\scripts\background -u 98 ..\..\bin\cfast fire_filterefficiency_half /V
..\scripts\background -u 98 ..\..\bin\cfast fire_CO_yield /V
..\scripts\background -u 98 ..\..\bin\cfast fire_soot_yield /V
cd ..
if %1==Fire goto end

:DOE
echo Running DOE Tests
cd Mass_Energy_Balance
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\..\bin\cfast DOE202 /V
cd ..
if %1==DOE goto end

:end
echo.| time
echo CFAST simulations complete.

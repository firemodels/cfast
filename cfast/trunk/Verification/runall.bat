echo off
echo.| time
echo Running CFAST simulations. $Rev: 846 $
if "%1"=="" goto Help
if %1==ALL goto All
if %1==Analytical goto Analytical
if %1==NoFire goto NoFire
if %1==Fire goto Fire
if %1==DOE goto DOE
:Help
echo Choose ALL, Analytical, Fire, DOE
goto end
:ALL
call cleanall.bat
:Analytical
echo Running Analytical Tests
cd Mass_Energy_Balance
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\..\bin\cfast basic_mechvent /V
..\..\bin\cfast 100kW_fire /V
cd ..
if %1==Analytical goto end

:NoFire
echo Running No Fire Tests
cd Mass_Energy_Balance
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\..\bin\cfast Base /V
..\..\bin\cfast basic_tempequilib /V
..\..\bin\cfast basic_tempequilib_wallsoff /V
..\..\bin\cfast basic_tempequilib_window /V
..\..\bin\cfast basic_tempequilib_window_elevation /V
..\..\bin\cfast basic_tempequilib_window_geometry /V
..\..\bin\cfast basic_tempequilib_window_wind /V
..\..\bin\cfast basic_pressure /V
..\..\bin\cfast basic_pressure_vent /V
..\..\bin\cfast basic_pressure_wallsoff /V
..\..\bin\cfast basic_mechvent_dropoff /V
..\..\bin\cfast basic_connection_floorceiling_mechvent /V
cd ..
if %1==NoFire goto end

:Fire
echo Running Fire Tests
cd Mass_Energy_Balance
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\..\bin\cfast fire /V
..\..\bin\cfast fire_window /V
..\..\bin\cfast fire_window_windowchange /V
..\..\bin\cfast fire_window_geochange /V
..\..\bin\cfast fire_ceiling /V
..\..\bin\cfast fire_ceiling_ventsize /V
..\..\bin\cfast fire_ceiling_geochange /V
..\..\bin\cfast fire_mechanical_vent /V
..\..\bin\cfast fire_HRRdoubled /V
..\..\bin\cfast fire_HRRarea2 /V
..\..\bin\cfast fire_sprinkler /V
..\..\bin\cfast fire_sprinkler_density /V
..\..\bin\cfast fire_sprinkler_RTI /V
..\..\bin\cfast fire_sprinkler_HRRdoubled /V
..\..\bin\cfast fire_hexane /V
..\..\bin\cfast fire_urethane /V
..\..\bin\cfast fire_hardwood /V
..\..\bin\cfast fire_ignitiontemp_120 /V
..\..\bin\cfast fire_ignitiontemp_700 /V
..\..\bin\cfast fire_filterefficiency /V
..\..\bin\cfast fire_filterefficiency_half /V
..\..\bin\cfast fire_CO_yield /V
..\..\bin\cfast fire_soot_yield /V
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

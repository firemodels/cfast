echo off
echo.| time
echo Running CFAST simulations. $Rev: 846 $
if "%1"=="" goto Help
if %1==ALL goto All
if %1==Base goto Base
if %1==Mass goto Mass
if %1==Energy goto Energy
if %1==NoFire goto NoFire
if %1==Fire goto Fire
if %1==NRC goto NRC
if %1==DOE goto DOE
:Help
echo Choose ALL, Analytical, Fire, DOE
goto end
:ALL
call cleanall.bat

:DOE
echo Running DOE Tests
cd DOE_Guidance_Report
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast DOE201 /V
..\scripts\background -u 98 ..\..\bin\cfast DOE202 /V
..\scripts\background -u 98 ..\..\bin\cfast DOE203 /V
..\scripts\background -u 98 ..\..\bin\cfast DOE204 /V
..\scripts\background -u 98 ..\..\bin\cfast DOE205 /V
..\scripts\background -u 98 ..\..\bin\cfast DOE206 /V
cd ..
if %1==DOE goto end

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

:Mass
echo Running Basic Mass Balance Tests
cd Analytical\Mass
if NOT %1==ALL call ..\..\..\Validation\cleancfast.bat
..\..\scripts\background -u 98 ..\..\..\bin\cfast basic_mechvent /V
cd ..\..
if %1==Mass goto end

:Energy
echo Running Basic Mass Balance Tests
cd Analytical
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast sealed_test /V
cd ..
if %1==Energy goto end

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
..\scripts\background -u 98 ..\..\bin\cfast fire_window_aspect_ratio /V
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
..\scripts\background -u 98 ..\..\bin\cfast fire_mechanical_vent_only /V
..\scripts\background -u 98 ..\..\bin\cfast fire_HRRarea1 /V
cd ..
if %1==Fire goto end

:NRC
echo Running NRC Tests
cd NRC_Users_Guide
if NOT %1==ALL call ..\..\Validation\cleancfast.bat
cd 1_Cabinet_Fire_in_MCR
..\..\scripts\background -u 98 ..\..\..\bin\cfast Cabinet_fire_in_MCR /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast Cabinet_fire_in_MCR_no_ventilation /V
cd ..
cd 2_Cabinet_Fire_in_Switchgear
..\..\scripts\background -u 98 ..\..\..\bin\cfast Initial_fire_only /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast Cabinet_fire_in_switchgear /V
cd ..
cd 4_MCC_Fire_in_Switchgear
..\..\scripts\background -u 98 ..\..\..\bin\cfast MCC_in_switchgear /V
cd ..
cd 5_Trash_Fire_in_Cable_Spreading_Room
..\..\scripts\background -u 98 ..\..\..\bin\cfast Trash_fire_in_cable_spreading_room /V
cd ..\..
if %1==NRC goto end

:end

echo Waiting for all CFAST runs to finish
:loop1
tasklist | find /i /c "CFAST" > temp.out
set /p numexe=<temp.out
echo Number of cases running - %numexe%
if %numexe% == 0 goto finished
Timeout /t 30 >nul 
goto loop1

:finished
echo.| time
echo CFAST simulations complete.

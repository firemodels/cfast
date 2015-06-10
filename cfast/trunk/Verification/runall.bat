echo off
echo.| time
echo Running CFAST Verification Cases

call cleanall.bat

echo Running Energy Balance cases
cd Energy_Balance
..\scripts\background -u 98 ..\..\bin\cfast sealed_test /V
cd ..

echo Running Mass Balance cases
cd Mass_Balance
..\scripts\background -u 98 ..\..\bin\cfast species_mass_1 /V
..\scripts\background -u 98 ..\..\bin\cfast species_mass_2 /V
..\scripts\background -u 98 ..\..\bin\cfast species_mass_3 /V
cd ..

echo Running Thermal Equilibrium cases
cd Thermal_Equilibrium
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib_window /V
..\scripts\background -u 98 ..\..\bin\cfast basic_tempequilib_window_elevation /V
cd ..

echo Running Ventilation cases
cd Ventilation
..\scripts\background -u 98 ..\..\bin\cfast ceiling_mechvent /V
cd ..

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
echo CFAST simulations complete

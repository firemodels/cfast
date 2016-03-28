echo off
echo.| time
echo Running CFAST Verification Cases

call cleanall.bat

echo Running Energy Balance cases
cd Energy_Balance
call ..\..\Validation\cleancfast.bat
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast sealed_test -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast sealed_test_2_layers -V
cd ..

echo Running Mass Balance cases
cd Mass_Balance
call ..\..\Validation\cleancfast.bat
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast species_mass_1 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast species_mass_2 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast species_mass_3 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast species_mass_4 -V
cd ..

echo Running Thermal Equilibrium cases
cd Thermal_Equilibrium
call ..\..\Validation\cleancfast.bat
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast basic_tempequilib -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast basic_tempequilib_window -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast basic_tempequilib_window_elevation -V
cd ..

echo Running Ventilation cases
cd Ventilation
call ..\..\Validation\cleancfast.bat
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast ventilation_1 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast ventilation_2 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast ventilation_3 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast ventilation_4 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast VVent_Tests -V
cd ..

echo Running Sprinkler cases
cd Sprinkler
call ..\..\Validation\cleancfast.bat
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast sprinkler_1 -V
cd ..

echo Running Radiation cases
cd Radiation
call ..\..\Validation\cleancfast.bat
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast radiation_1 -V
background -u 98 ..\..\Utilities\for_bundle\Bin\cfast radiation_2 -V
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

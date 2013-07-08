echo off
echo Removing old and temporary files.  Messages can be ignored here.
del *_n.csv /s/q >nul
del *_s.csv /s/q >nul
del *_f.csv /s/q >nul
del *_w.csv /s/q >nul
del *_zone.csv /s/q >nul
del *.out /s/q >nul
del *.smv /s/q >nul
del *.plt /s/q >nul
del *.status /s/q >nul
del *.log /s/q >nul
del ..\Docs\Validation_Guide\FIGURES\ATF_Corridors\ATF_Corridors_HGL*kW.pdf
del ..\Docs\Validation_Guide\FIGURES\ATF_Corridors\ATF_Corridors_Jet*kW.pdf
del ..\Docs\Validation_Guide\FIGURES\FM_NBS\fm??_*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\FM_SNL\FM_SNL_*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\High_Bay\USN_Hawaii_Test_??*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\High_Bay\USN_Iceland_Test_??*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\iBMB\iBMB_Cable_*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\iBMB\iBMB_Pool_*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\LLNL_Enclosure\LLNL_??_*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\NBS\1r*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\NBS\NBS_100*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\NIST_Dunes_2000\NIST_Dunes_2000_SD*.pdf
del ..\Docs\Validation_Guide\FIGURES\NIST_NRC\NIST_NRC_??_*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\NIST_PLAZA\Room*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\Steckler_Compartment\Steckler_???*.pdf /q >nul
del ..\Docs\Validation_Guide\FIGURES\Vettori_Flat\Activation_Test_??.pdf
del ..\Docs\Validation_Guide\FIGURES\Vettori_Flat\Test_??.pdf
del ..\Docs\Validation_Guide\FIGURES\VTT\VTT_??_*.pdf /q >nul
echo Done removing old and temporary files.
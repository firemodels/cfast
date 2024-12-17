@echo off
echo.| time
echo Running CFAST simulations
if "%1"=="" goto Help
if %1==ALL goto ALL
if %1==ATF goto ATF
if %1==DelCo_Trainers goto DelCo_Trainers
if %1==Dunes_2000 goto Dunes2000
if %1==Fleury_Heat_Flux goto FLeury_Heat_Flux
if %1==FM_NBS goto FM_NBS
if %1==FM_SNL goto FM_SNL
if %1==High_Bay goto High_Bay
if %1==iBMB goto iBMB
if %1==LLNL_Enclosure goto LLNL_Enclosure
if %1==NBS goto NBS
if %1==NBS_1Room goto NBS_1Room
if %1==NIST_NRC goto NIST_NRC
if %1==NIST_NRC_Corner_Effects goto NIST_NRC_Corner_Effects
if %1==NIST_Vent_Study goto NIST_Vent_Study
if %1==PLAZA goto PLAZA
if %1==PRISME goto PRISME
if %1==SP_AST goto SP_AST
if %1==Steckler_Compartment goto Steckler
if %1==UL_NFPRF goto UL_NFPRF
if %1==UL_NIJ_Houses goto UL_NIJ_Houses
if %1==UL_NIST_Vents goto UL_NIST_Vents
if %1==Vettori_Flat goto Vettori_Flat
if %1==VTT goto VTT
if %1==WTC goto WTC
:Help
echo Choose ALL, ATF, Dunes_2000, FLeury_Heat_Flux, FM_NBS, FM_SNL, High_Bay, iBMB, LLNL_Enclosure,
echo        NBS, NBS_1Room, NIST_NRC, NIST_Corner_Effects, NIST_Vent_Study, PLAZA, PRISME, 
echo        Steckler_Compartment, UL_NFPRF, UL_NIJ_Houses, UL_NIST_Vents, Vettori_Flat, VTT, or WTC
goto end
:ALL
call cleanall.bat

:ATF
echo ATF Corridor Tests
cd ATF_Corridors
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe ATF_Corridors_050_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe ATF_Corridors_100_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe ATF_Corridors_240_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe ATF_Corridors_250_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe ATF_Corridors_500_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe ATF_Corridors_Mix_kW -V
cd ..
if %1==ATF goto end

:DelCo_Trainers
echo DelCo Trainers Tests
cd DelCo_Trainers
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_03 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_04 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_06 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_22 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_23 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_24 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_25 -V
cd ..
if %1==DelCo_Trainers goto end

:Fleury_Heat_Flux
echo Fleury Heat Flux Tests
cd Fleury_Heat_Flux
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_1t1_100_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_1t1_150_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_1t1_200_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_1t1_250_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_1t1_300_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_2t1_100_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_2t1_150_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_2t1_200_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_2t1_250_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_2t1_300_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_3t1_100_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_3t1_150_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_3t1_200_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_3t1_250_kW -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Fleury_3t1_300_kW -V
cd ..
if %1==Fleury_Heat_Flux goto end

:FM_NBS
echo FM NBS 4 room tests 19, 21
cd fm_nbs
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe fm19 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe fm21 -V
cd ..\
if %1==FM_NBS goto end

:FM_SNL
echo FM SNL Tests
cd FM_SNL
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_3 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_4 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_5 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_6 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_7 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_8 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_9 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_12 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_13 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_14 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_15 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_16 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_17 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_21 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe FMSNL_22 -V
cd ..\
if %1==FM_SNL goto end

:High_Bay
echo High Bay Tests
cd High_Bay
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_01 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_03 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_04 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_06 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_07 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Hawaii_Test_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_01 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_03 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_04 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_06 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_07 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_09 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_12 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_13 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_14 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_15 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_17 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_18 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_19 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe USN_Iceland_Test_20 -V
cd ..\
if %1==High_Bay goto end

:iBMB
echo iBMB_4 Test 1
cd iBMB_4\
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe iBMB_4_T1 -V
cd ..\
echo iBMB_5 Test 4
cd iBMB_5\
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe iBMB_5_T4 -V
cd ..\
if %1==iBMB goto end

:LLNL_Enclosure
echo LLNL Tests
cd LLNL_Enclosure
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_01 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_03 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_04 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_06 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_07 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_08 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_09 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_12 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_13 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_14 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_15 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_16 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_17 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_18 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_19 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_20 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_21 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_22 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_23 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_24 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_25 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_26 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_27 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_28 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_29 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_30 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_31 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_32 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_33 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_34 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_35 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_36 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_37 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_38 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_39 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_40 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_41 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_42 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_43 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_44 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_45 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_46 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_47 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_48 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_49 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_50 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_51 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_52 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_53 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_54 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_55 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_56 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_57 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_58 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_59 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_60 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_61 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_62 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_63 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe LLNL_64 -V
cd ..\
if %1==LLNL_Enclosure goto end

:NBS
echo NBS Tests MV100A, MV100O, MV100Z
cd NBS
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe MV100A -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe MV100O -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe MV100Z -V
cd ..\
if %1==NBS goto end

:NBS_1Room
echo NBS 1 room furniture tests 1, 6
cd 1rfurn
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe 1rfurn1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe 1rfurn6 -V
cd ..\
echo NBS 1 room wall burning test 1, 2
cd 1rwall
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe 1rwall1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe 1rwall2 -V
cd ..\
if %1==NBS_1Room goto end

:PLAZA
echo NBS Plaza Hotel test 7
cd Multi
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Multi -V
cd ..\
if %1==PLAZA goto end

:Dunes2000
echo NIST Dunes 2000 tests
cd NIST_Dunes_2000
call ..\CleanCFAST
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC07 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC33 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC35 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC38 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_Dunes_2000_SDC39 -V
cd ..\
if %1==Dunes_2000 goto end

:NIST_NRC
cd NIST_NRC
call ..\cleancfast.bat
echo NIST_NRC tests 1-5, 7-10, 13-18
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T3 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T4 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T5 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T7 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T8 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T9 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T13 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T14 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T15 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T16 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T17 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe NIST_NRC_T18 -V
cd ..\
if %1==NIST_NRC goto end


:NIST_NRC_Corner_Effects
cd NIST_NRC_Corner_Effects
call ..\cleancfast.bat
echo NIST_NRC Corner effects Tests
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe corner_200_kW.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe corner_300_kW.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe corner_400_kW.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe wall_200_kW.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe wall_300_kW.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe wall_400_kW.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_01.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_02.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_03.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_04.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_05.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_06.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_07.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_08.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_09.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_10.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_11.in -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe cabinet_12.in -V
cd ..\
if %1==NIST_NRC_Corner_Effects goto end
:NIST_Vent_Study
echo NIST Vent Study
cd NIST_Vent_Study
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_3 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_4 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_5 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_6 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_7 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_8 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_9 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_12 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_13 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_14 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_15 -V
cd ..
if %1==NIST_Vent_Study goto end

:PRISME
echo PRISME
cd PRISME
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe PRS_D1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe PRS_D2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe PRS_D3 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe PRS_D4 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe PRS_D5 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe PRS_D6 -V
cd ..
if %1==PRISME goto end

:SP_AST
echo SP_AST Tests
cd SP_AST
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe SP_AST_Test_1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe SP_AST_Test_2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe SP_AST_Test_3 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe SP_AST_Diesel_1p1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe SP_AST_Diesel_1p9 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe SP_AST_Heptane_1p1 -V
cd ..
if %1==SP_AST goto end

:Steckler
echo Steckler Compartment Tests
cd Steckler_Compartment
call ..\CleanCFAST
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_010 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_011 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_012 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_612 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_013 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_014 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_018 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_710 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_810 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_016 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_017 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_022 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_023 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_030 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_041 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_019 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_020 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_021 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_114 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_144 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_212 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_242 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_410 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_210 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_310 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_240 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_116 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_122 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_224 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_324 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_220 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_221 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_514 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_544 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_512 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_542 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_610 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_510 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_540 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_517 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_622 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_522 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_524 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_541 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_520 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_521 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_513 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_160 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_163 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_164 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_165 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_162 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_167 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_161 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Steckler_166 -V
cd ..\
if %1==Steckler_Compartment goto end

:UL_NFPRF
echo UL_NFPRF Series I
cd UL_NFPRF
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_01 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_03 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_04 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_06 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_07 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_08 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_09 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_12 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_13 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_14 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_15 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_16 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_17 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_18 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_19 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_20 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_21 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_1_22 -V

echo UL_NFPRF Series II
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_01 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_03 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_04 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_06 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_07 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_08 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_09 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NFPRF_2_12 -V
cd ..
if %1==UL_NFPRF goto end

:UL_NIJ_Houses
echo UL_NIJ
cd UL_NIJ_Houses
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Single_Story_Gas_1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Single_Story_Gas_2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Single_Story_Gas_5 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Two_Story_Gas_1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Two_Story_Gas_4 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Two_Story_Gas_6 -V
cd ..
if %1==UL_NIJ_Houses goto end

:UL_NIST_Vents
echo UL_NIST_Vents
cd UL_NIST_Vents
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NIST_Vents_Test_1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NIST_Vents_Test_2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NIST_Vents_Test_3 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe UL_NIST_Vents_Test_4 -V
cd ..
if %1==UL_NIST_Vents goto end

:Vettori_Flat
echo Vettori Flat Simulations
cd Vettori_Flat
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_3 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_4 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_5 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_6 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_7 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_8 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_9 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_10 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_11 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_12 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_13 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_14 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_15 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_16 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_17 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_18 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_19 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_20 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_21 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_22 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_23 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_24 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_25 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_26 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_27 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_28 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_29 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_30 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_31 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_32 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_33 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_34 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_35 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_36 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_37 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_38 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_39 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_40 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_41 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_42 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_43 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_44 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe Test_45 -V
cd ..
if %1==Vettori_Flat goto end

:VTT
cd VTT
echo VTT Cases 1, 2, 3
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe VTT_C1 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe VTT_C2 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe VTT_C3 -V
cd ..\
if %1==VTT goto end

:WTC
echo WTC Spray Burner Tests
cd WTC
call ..\cleancfast.bat
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe WTC_01 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe WTC_02 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe WTC_03 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe WTC_04 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe WTC_05 -V
background -U 11 ..\..\Utilities\for_bundle\Bin\cfast.exe WTC_06 -V
cd ..
if %1==WTC goto end


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

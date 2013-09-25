@echo off

set curdir=%CD%
set cfastroot=%curdir%\..
set background="%cfastroot%\Validation\scripts\background.exe" -u 99
Rem set cfast="%cfastroot%\bin\"cfast.exe
set cfastsourcedir=%cfastroot%\CFAST\intel_win_64
set cfast=%cfastsourcedir%\cfast6_win_64.exe


Rem Build cfast

echo Building cfast

cd "%cfastsourcedir%"
erase *.obj *.mod
call make_cfast

cd "%curdir%"

echo.| time
echo Running CFAST simulations. $Rev: 1207 $
if "%1"=="" goto Help
if %1==ALL goto ALL
if %1==ATF goto ATF
if %1==Vettori_Flat goto Vettori_Flat
if %1==LLNL_Enclosure goto LLNL_Enclosure
if %1==NBS_1Room goto NBS_1Room
if %1==FM_NBS goto FM_NBS
if %1==PLAZA goto PLAZA
if %1==VTT goto VTT
if %1==NIST_NRC goto NIST_NRC
if %1==iBMB goto iBMB
if %1==FM_SNL goto FM_SNL
if %1==NBS goto NBS
if %1==High_Bay goto High_Bay
if %1==WTC goto WTC
if %1==Steckler_Compartment goto Steckler
if %1==Dunes_2000 goto Dunes2000
if %1==SP_AST goto SP_AST
if %1==UL_NFPRF goto UL_NFPRF
:Help
echo Choose ALL, ATF, Dunes_2000, FM_NBS, FM_SNL, High_Bay, iBMB, LLNL_Enclosure,
echo        NBS, NBS_1Room, NIST_NRC, Steckler_Compartment, Vettori_Flat, VTT, or WTC
goto end
:ALL
call cleanall.bat
:UL_NFPRF
echo Running UL_NFPRF Series I
cd UL_NFPRF
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% UL_NFPRF_1_01 /V
%background% %cfast% UL_NFPRF_1_02 /V
%background% %cfast% UL_NFPRF_1_03 /V
%background% %cfast% UL_NFPRF_1_04 /V
%background% %cfast% UL_NFPRF_1_05 /V
%background% %cfast% UL_NFPRF_1_06 /V
%background% %cfast% UL_NFPRF_1_07 /V
%background% %cfast% UL_NFPRF_1_08 /V
%background% %cfast% UL_NFPRF_1_09 /V
%background% %cfast% UL_NFPRF_1_10 /V
%background% %cfast% UL_NFPRF_1_11 /V
%background% %cfast% UL_NFPRF_1_12 /V
%background% %cfast% UL_NFPRF_1_13 /V
%background% %cfast% UL_NFPRF_1_14 /V
%background% %cfast% UL_NFPRF_1_15 /V
%background% %cfast% UL_NFPRF_1_16 /V
%background% %cfast% UL_NFPRF_1_17 /V
%background% %cfast% UL_NFPRF_1_18 /V
%background% %cfast% UL_NFPRF_1_19 /V
%background% %cfast% UL_NFPRF_1_20 /V
%background% %cfast% UL_NFPRF_1_21 /V
%background% %cfast% UL_NFPRF_1_22 /V
cd ..
if %1==UL_NFPRF goto end

:SP_AST
echo Running SP_AST Tests
cd SP_AST
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% SP_AST_Test_1 /V
%background% %cfast% SP_AST_Test_2 /V
%background% %cfast% SP_AST_Test_3 /V
cd ..
if %1==SP_AST goto end

:ATF
echo Running ATF Corridor Tests
cd ATF_Corridors
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% ATF_Corridors_050_kW /V
%background% %cfast% ATF_Corridors_100_kW /V
%background% %cfast% ATF_Corridors_240_kW /V
%background% %cfast% ATF_Corridors_250_kW /V
%background% %cfast% ATF_Corridors_500_kW /V
%background% %cfast% ATF_Corridors_Mix_kW /V
cd ..
if %1==ATF goto end
:WTC
echo Running WTC Spray Burner Tests
cd WTC
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% WTC_01 /V
%background% %cfast% WTC_02 /V
%background% %cfast% WTC_03 /V
%background% %cfast% WTC_04 /V
%background% %cfast% WTC_05 /V
%background% %cfast% WTC_06 /V
cd ..
if %1==WTC goto end
:Vettori_Flat
echo Running Vettori Flat Simulations
cd Vettori_Flat
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% Test_1 /V
%background% %cfast% Test_2 /V
%background% %cfast% Test_3 /V
%background% %cfast% Test_4 /V
%background% %cfast% Test_5 /V
%background% %cfast% Test_6 /V
%background% %cfast% Test_7 /V
%background% %cfast% Test_8 /V
%background% %cfast% Test_9 /V
%background% %cfast% Test_10 /V
%background% %cfast% Test_11 /V
%background% %cfast% Test_12 /V
%background% %cfast% Test_13 /V
%background% %cfast% Test_14 /V
%background% %cfast% Test_15 /V
%background% %cfast% Test_16 /V
%background% %cfast% Test_17 /V
%background% %cfast% Test_18 /V
%background% %cfast% Test_19 /V
%background% %cfast% Test_20 /V
%background% %cfast% Test_21 /V
%background% %cfast% Test_22 /V
%background% %cfast% Test_23 /V
%background% %cfast% Test_24 /V
%background% %cfast% Test_25 /V
%background% %cfast% Test_26 /V
%background% %cfast% Test_27 /V
%background% %cfast% Test_28 /V
%background% %cfast% Test_29 /V
%background% %cfast% Test_30 /V
%background% %cfast% Test_31 /V
%background% %cfast% Test_32 /V
%background% %cfast% Test_33 /V
%background% %cfast% Test_34 /V
%background% %cfast% Test_35 /V
%background% %cfast% Test_36 /V
%background% %cfast% Test_37 /V
%background% %cfast% Test_38 /V
%background% %cfast% Test_39 /V
%background% %cfast% Test_40 /V
%background% %cfast% Test_41 /V
%background% %cfast% Test_42 /V
%background% %cfast% Test_43 /V
%background% %cfast% Test_44 /V
%background% %cfast% Test_45 /V
cd ..
if %1==Vettori_Flat goto end
:LLNL_Enclosure
echo LLNL Tests
cd LLNL_Enclosure
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% LLNL_01 /V
%background% %cfast% LLNL_02 /V
%background% %cfast% LLNL_03 /V
%background% %cfast% LLNL_04 /V
%background% %cfast% LLNL_05 /V
%background% %cfast% LLNL_06 /V
%background% %cfast% LLNL_07 /V
%background% %cfast% LLNL_08 /V
%background% %cfast% LLNL_09 /V
%background% %cfast% LLNL_10 /V
%background% %cfast% LLNL_11 /V
%background% %cfast% LLNL_12 /V
%background% %cfast% LLNL_13 /V
%background% %cfast% LLNL_14 /V
%background% %cfast% LLNL_15 /V
%background% %cfast% LLNL_16 /V
%background% %cfast% LLNL_17 /V
%background% %cfast% LLNL_18 /V
%background% %cfast% LLNL_19 /V
%background% %cfast% LLNL_20 /V
%background% %cfast% LLNL_21 /V
%background% %cfast% LLNL_22 /V
%background% %cfast% LLNL_23 /V
%background% %cfast% LLNL_24 /V
%background% %cfast% LLNL_25 /V
%background% %cfast% LLNL_26 /V
%background% %cfast% LLNL_27 /V
%background% %cfast% LLNL_28 /V
%background% %cfast% LLNL_29 /V
%background% %cfast% LLNL_30 /V
%background% %cfast% LLNL_31 /V
%background% %cfast% LLNL_32 /V
%background% %cfast% LLNL_33 /V
%background% %cfast% LLNL_34 /V
%background% %cfast% LLNL_35 /V
%background% %cfast% LLNL_36 /V
%background% %cfast% LLNL_37 /V
%background% %cfast% LLNL_38 /V
%background% %cfast% LLNL_39 /V
%background% %cfast% LLNL_40 /V
%background% %cfast% LLNL_41 /V
%background% %cfast% LLNL_42 /V
%background% %cfast% LLNL_43 /V
%background% %cfast% LLNL_44 /V
%background% %cfast% LLNL_45 /V
%background% %cfast% LLNL_46 /V
%background% %cfast% LLNL_47 /V
%background% %cfast% LLNL_48 /V
%background% %cfast% LLNL_49 /V
%background% %cfast% LLNL_50 /V
%background% %cfast% LLNL_51 /V
%background% %cfast% LLNL_52 /V
%background% %cfast% LLNL_53 /V
%background% %cfast% LLNL_54 /V
%background% %cfast% LLNL_55 /V
%background% %cfast% LLNL_56 /V
%background% %cfast% LLNL_57 /V
%background% %cfast% LLNL_58 /V
%background% %cfast% LLNL_59 /V
%background% %cfast% LLNL_60 /V
%background% %cfast% LLNL_61 /V
%background% %cfast% LLNL_62 /V
%background% %cfast% LLNL_63 /V
%background% %cfast% LLNL_64 /V
cd ..\
if %1==LLNL_Enclosure goto end
:NBS_1Room
echo NBS 1 room furniture tests 1, 6
cd 1rfurn
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% 1rfurn1 /V
%background% %cfast% 1rfurn6 /V
cd ..\
echo NBS 1 room wall burning test 1, 2
cd 1rwall
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% 1rwall1 /V
%background% %cfast% 1rwall2 /V
cd ..\
if %1==NBS_1Room goto end
:FM_NBS
echo FM NBS 4 room tests 19, 21
cd fm_nbs
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% fm19 /V
%background% %cfast% fm21 /V
cd ..\
if %1==FM_NBS goto end
:PLAZA
echo NBS Plaza Hotel test 7
cd Multi
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% Multi /V
cd ..\
if %1==PLAZA goto end
:VTT
cd VTT
echo VTT Cases 1, 2, 3
if NOT %1==ALL call ..\cleancfast.bat
cd Case_1
%background% %cfast% VTT_C1 /V
cd ..\
cd Case_2
%background% %cfast% VTT_C2 /V
cd ..\
cd Case_3
%background% %cfast% VTT_C3 /V
cd ..\..
if %1==VTT goto end
:NIST_NRC
cd NIST_NRC
if NOT %1==ALL call ..\cleancfast.bat
echo NIST_NRC tests 1-5, 7-10, 13-18
cd Test_1\
%background% %cfast% NIST_NRC_T1 /V
cd ..\
cd Test_2\
%background% %cfast% NIST_NRC_T2 /V
cd ..\
cd Test_3\
%background% %cfast% NIST_NRC_T3 /V
cd ..\
cd Test_4\
%background% %cfast% NIST_NRC_T4 /V
cd ..\
cd Test_5\
%background% %cfast% NIST_NRC_T5 /V
cd ..\
cd Test_7\
%background% %cfast% NIST_NRC_T7 /V
cd ..\
cd Test_8\
%background% %cfast% NIST_NRC_T8 /V
cd ..\
cd Test_9\
%background% %cfast% NIST_NRC_T9 /V
cd ..\
cd Test_10\
%background% %cfast% NIST_NRC_T10 /V
cd ..\
cd Test_13\
%background% %cfast% NIST_NRC_T13 /V
cd ..\
cd Test_14\
%background% %cfast% NIST_NRC_T14 /V
cd ..\
cd Test_15\
%background% %cfast% NIST_NRC_T15 /V
cd ..\
cd Test_16\
%background% %cfast% NIST_NRC_T16 /V
cd ..\
cd Test_17\
%background% %cfast% NIST_NRC_T17 /V
cd ..\
cd Test_18\
%background% %cfast% NIST_NRC_T18 /V
cd ..\..
if %1==NIST_NRC goto end
:iBMB
echo iBMB_4 Test 1
cd iBMB_4\
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% iBMB_4_T1 /V
cd ..\
echo iBMB_5 Test 4
cd iBMB_5\
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% iBMB_5_T4 /V
cd ..\
if %1==iBMB goto end
:FM_SNL
echo FM SNL Tests
cd FM_SNL
if NOT %1==ALL call ..\cleancfast.bat
cd Test_1
%background% %cfast% FMSNL_1 /V
cd ..\
cd Test_2
%background% %cfast% FMSNL_2 /V
cd ..\
cd Test_3
%background% %cfast% FMSNL_3 /V
cd ..\
cd Test_4
%background% %cfast% FMSNL_4 /V
cd ..\
cd Test_5
%background% %cfast% FMSNL_5 /V
cd ..\
cd Test_6
%background% %cfast% FMSNL_6 /V
cd ..\
cd Test_7
%background% %cfast% FMSNL_7 /V
cd ..\
cd Test_8
%background% %cfast% FMSNL_8 /V
cd ..\
cd Test_9
%background% %cfast% FMSNL_9 /V
cd ..\
cd Test_10
%background% %cfast% FMSNL_10 /V
cd ..\
cd Test_11
%background% %cfast% FMSNL_11 /V
cd ..\
cd Test_12
%background% %cfast% FMSNL_12 /V
cd ..\
cd Test_13
%background% %cfast% FMSNL_13 /V
cd ..\
cd Test_14
%background% %cfast% FMSNL_14 /V
cd ..\
cd Test_15
%background% %cfast% FMSNL_15 /V
cd ..\
cd Test_16
%background% %cfast% FMSNL_16 /V
cd ..\
cd Test_17
%background% %cfast% FMSNL_17 /V
cd ..\
cd Test_21
%background% %cfast% FMSNL_21 /V
cd ..\
cd Test_22
%background% %cfast% FMSNL_22 /V
cd ..\..\
if %1==FM_SNL goto end
:NBS
echo NBS Tests MV100A, MV100O, MV100Z
cd NBS
if NOT %1==ALL call ..\cleancfast.bat
cd MV100A
%background% %cfast% MV100A /V
cd ..\
cd MV100O
%background% %cfast% MV100O /V
cd ..\
cd MV100Z
%background% %cfast% MV100Z /V
cd ..\..\
if %1==NBS goto end
:High_Bay
echo High Bay Tests
cd High_Bay
if NOT %1==ALL call ..\cleancfast.bat
%background% %cfast% USN_Hawaii_Test_01 /V
%background% %cfast% USN_Hawaii_Test_02 /V
%background% %cfast% USN_Hawaii_Test_03 /V
%background% %cfast% USN_Hawaii_Test_04 /V
%background% %cfast% USN_Hawaii_Test_05 /V
%background% %cfast% USN_Hawaii_Test_06 /V
%background% %cfast% USN_Hawaii_Test_07 /V
%background% %cfast% USN_Hawaii_Test_11 /V
%background% %cfast% USN_Iceland_Test_01 /V
%background% %cfast% USN_Iceland_Test_02 /V
%background% %cfast% USN_Iceland_Test_03 /V
%background% %cfast% USN_Iceland_Test_04 /V
%background% %cfast% USN_Iceland_Test_05 /V
%background% %cfast% USN_Iceland_Test_06 /V
%background% %cfast% USN_Iceland_Test_07 /V
%background% %cfast% USN_Iceland_Test_09 /V
%background% %cfast% USN_Iceland_Test_10 /V
%background% %cfast% USN_Iceland_Test_11 /V
%background% %cfast% USN_Iceland_Test_12 /V
%background% %cfast% USN_Iceland_Test_13 /V
%background% %cfast% USN_Iceland_Test_14 /V
%background% %cfast% USN_Iceland_Test_15 /V
%background% %cfast% USN_Iceland_Test_17 /V
%background% %cfast% USN_Iceland_Test_18 /V
%background% %cfast% USN_Iceland_Test_19 /V
%background% %cfast% USN_Iceland_Test_20 /V
cd ..\
if %1==High_Bay goto end
:Steckler
echo Steckler Compartment Tests
cd Steckler_Compartment
if NOT %1==ALL call ..\CleanCFAST
%background% %cfast% Steckler_010 /V
%background% %cfast% Steckler_011 /V
%background% %cfast% Steckler_012 /V
%background% %cfast% Steckler_612 /V
%background% %cfast% Steckler_013 /V
%background% %cfast% Steckler_014 /V
%background% %cfast% Steckler_018 /V
%background% %cfast% Steckler_710 /V
%background% %cfast% Steckler_810 /V
%background% %cfast% Steckler_016 /V
%background% %cfast% Steckler_017 /V
%background% %cfast% Steckler_022 /V
%background% %cfast% Steckler_023 /V
%background% %cfast% Steckler_030 /V
%background% %cfast% Steckler_041 /V
%background% %cfast% Steckler_019 /V
%background% %cfast% Steckler_020 /V
%background% %cfast% Steckler_021 /V
%background% %cfast% Steckler_114 /V
%background% %cfast% Steckler_144 /V
%background% %cfast% Steckler_212 /V
%background% %cfast% Steckler_242 /V
%background% %cfast% Steckler_410 /V
%background% %cfast% Steckler_210 /V
%background% %cfast% Steckler_310 /V
%background% %cfast% Steckler_240 /V
%background% %cfast% Steckler_116 /V
%background% %cfast% Steckler_122 /V
%background% %cfast% Steckler_224 /V
%background% %cfast% Steckler_324 /V
%background% %cfast% Steckler_220 /V
%background% %cfast% Steckler_221 /V
%background% %cfast% Steckler_514 /V
%background% %cfast% Steckler_544 /V
%background% %cfast% Steckler_512 /V
%background% %cfast% Steckler_542 /V
%background% %cfast% Steckler_610 /V
%background% %cfast% Steckler_510 /V
%background% %cfast% Steckler_540 /V
%background% %cfast% Steckler_517 /V
%background% %cfast% Steckler_622 /V
%background% %cfast% Steckler_522 /V
%background% %cfast% Steckler_524 /V
%background% %cfast% Steckler_541 /V
%background% %cfast% Steckler_520 /V
%background% %cfast% Steckler_521 /V
%background% %cfast% Steckler_513 /V
%background% %cfast% Steckler_160 /V
%background% %cfast% Steckler_163 /V
%background% %cfast% Steckler_164 /V
%background% %cfast% Steckler_165 /V
%background% %cfast% Steckler_162 /V
%background% %cfast% Steckler_167 /V
%background% %cfast% Steckler_161 /V
%background% %cfast% Steckler_166 /V
cd ..\
if %1==Steckler_Compartment goto end
:Dunes2000
echo NIST Dunes 2000 tests
cd NIST_Dunes_2000
if NOT %1==ALL call ..\CleanCFAST
%background% %cfast% NIST_Dunes_2000_SDC02 /V
%background% %cfast% NIST_Dunes_2000_SDC05 /V
%background% %cfast% NIST_Dunes_2000_SDC07 /V
%background% %cfast% NIST_Dunes_2000_SDC10 /V
%background% %cfast% NIST_Dunes_2000_SDC33 /V
%background% %cfast% NIST_Dunes_2000_SDC35 /V
%background% %cfast% NIST_Dunes_2000_SDC38 /V
%background% %cfast% NIST_Dunes_2000_SDC39 /V
cd ..\
if %1==Dunes2000 goto end
:end
echo.| time
echo CFAST simulations complete.
echo If necessary, run the following to create profiles and corrected outputs once all model runs are complete
echo
echo ..\VandV_Calcs\Release\VandV_Calcs.exe CFAST_Pressure_Correction_Inputs.csv
echo copy pressures.csv LLNL_Enclosure\LLNL_pressures.csv /Y
echo ..\VandV_Calcs\Release\VandV_Calcs.exe CFAST_Temperature_Profile_inputs.csv
echo copy profiles.csv Steckler_Compartment /Y

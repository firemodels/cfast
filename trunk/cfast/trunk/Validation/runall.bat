@echo off
echo.| time
echo Running CFAST simulations. $Rev$
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
if %1==UL_NIST_Vents goto UL_NIST_Vents
:Help
echo Choose ALL, ATF, Dunes_2000, FM_NBS, FM_SNL, High_Bay, iBMB, LLNL_Enclosure,
echo        NBS, NBS_1Room, NIST_NRC, Steckler_Compartment, UL_NFPRF, UL_NIST_Vents, 
echo        Vettori_Flat, VTT, PLAZA, or WTC
goto end
:ALL
call cleanall.bat

:UL_NIST_Vents
echo Running UL_NIST_Vents
cd UL_NIST_Vents
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NIST_Vents_Test_1 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NIST_Vents_Test_2 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NIST_Vents_Test_3 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NIST_Vents_Test_4 /V
cd ..
if %1==UL_NIST_Vents goto end

:UL_NFPRF
echo Running UL_NFPRF Series I
cd UL_NFPRF
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_01 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_02 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_03 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_04 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_05 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_06 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_07 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_08 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_09 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_10 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_11 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_12 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_13 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_14 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_15 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_16 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_17 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_18 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_19 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_20 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_21 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_1_22 /V

echo Running UL_NFPRF Series II
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_01 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_02 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_03 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_04 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_05 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_06 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_07 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_08 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_09 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_10 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_11 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe UL_NFPRF_2_12 /V
cd ..
if %1==UL_NFPRF goto end

:SP_AST
echo Running SP_AST Tests
cd SP_AST
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe SP_AST_Test_1 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe SP_AST_Test_2 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe SP_AST_Test_3 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe SP_AST_Diesel_1p1 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe SP_AST_Diesel_1p9 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe SP_AST_Heptane_1p1 /V
cd ..
if %1==SP_AST goto end

:ATF
echo Running ATF Corridor Tests
cd ATF_Corridors
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe ATF_Corridors_050_kW /V
..\scripts\background -u 98 ..\..\bin\cfast.exe ATF_Corridors_100_kW /V
..\scripts\background -u 98 ..\..\bin\cfast.exe ATF_Corridors_240_kW /V
..\scripts\background -u 98 ..\..\bin\cfast.exe ATF_Corridors_250_kW /V
..\scripts\background -u 98 ..\..\bin\cfast.exe ATF_Corridors_500_kW /V
..\scripts\background -u 98 ..\..\bin\cfast.exe ATF_Corridors_Mix_kW /V
cd ..
if %1==ATF goto end

:WTC
echo Running WTC Spray Burner Tests
cd WTC
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe WTC_01 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe WTC_02 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe WTC_03 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe WTC_04 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe WTC_05 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe WTC_06 /V
cd ..
if %1==WTC goto end

:Vettori_Flat
echo Running Vettori Flat Simulations
cd Vettori_Flat
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_1 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_2 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_3 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_4 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_5 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_6 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_7 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_8 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_9 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_10 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_11 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_12 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_13 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_14 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_15 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_16 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_17 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_18 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_19 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_20 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_21 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_22 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_23 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_24 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_25 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_26 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_27 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_28 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_29 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_30 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_31 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_32 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_33 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_34 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_35 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_36 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_37 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_38 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_39 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_40 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_41 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_42 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_43 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_44 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Test_45 /V
cd ..
if %1==Vettori_Flat goto end

:LLNL_Enclosure
echo LLNL Tests
cd LLNL_Enclosure
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_01 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_02 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_03 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_04 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_05 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_06 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_07 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_08 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_09 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_10 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_11 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_12 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_13 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_14 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_15 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_16 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_17 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_18 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_19 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_20 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_21 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_22 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_23 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_24 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_25 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_26 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_27 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_28 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_29 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_30 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_31 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_32 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_33 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_34 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_35 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_36 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_37 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_38 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_39 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_40 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_41 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_42 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_43 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_44 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_45 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_46 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_47 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_48 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_49 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_50 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_51 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_52 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_53 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_54 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_55 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_56 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_57 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_58 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_59 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_60 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_61 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_62 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_63 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe LLNL_64 /V
cd ..\
if %1==LLNL_Enclosure goto end

:NBS_1Room
echo NBS 1 room furniture tests 1, 6
cd 1rfurn
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe 1rfurn1 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe 1rfurn6 /V
cd ..\
echo NBS 1 room wall burning test 1, 2
cd 1rwall
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe 1rwall1 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe 1rwall2 /V
cd ..\
if %1==NBS_1Room goto end
:FM_NBS
echo FM NBS 4 room tests 19, 21
cd fm_nbs
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe fm19 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe fm21 /V
cd ..\
if %1==FM_NBS goto end

:VTT
cd VTT
echo VTT Cases 1, 2, 3
call ..\cleancfast.bat
cd Case_1
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe VTT_C1 /V
cd ..\
cd Case_2
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe VTT_C2 /V
cd ..\
cd Case_3
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe VTT_C3 /V
cd ..\..
if %1==VTT goto end

:NIST_NRC
cd NIST_NRC
call ..\cleancfast.bat
echo NIST_NRC tests 1-5, 7-10, 13-18
cd Test_1\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T1 /V
cd ..\
cd Test_2\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T2 /V
cd ..\
cd Test_3\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T3 /V
cd ..\
cd Test_4\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T4 /V
cd ..\
cd Test_5\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T5 /V
cd ..\
cd Test_7\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T7 /V
cd ..\
cd Test_8\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T8 /V
cd ..\
cd Test_9\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T9 /V
cd ..\
cd Test_10\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T10 /V
cd ..\
cd Test_13\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T13 /V
cd ..\
cd Test_14\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T14 /V
cd ..\
cd Test_15\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T15 /V
cd ..\
cd Test_16\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T16 /V
cd ..\
cd Test_17\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T17 /V
cd ..\
cd Test_18\
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe NIST_NRC_T18 /V
cd ..\..
if %1==NIST_NRC goto end

:iBMB
echo iBMB_4 Test 1
cd iBMB_4\
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe iBMB_4_T1 /V
cd ..\
echo iBMB_5 Test 4
cd iBMB_5\
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe iBMB_5_T4 /V
cd ..\
if %1==iBMB goto end

:FM_SNL
echo FM SNL Tests
cd FM_SNL
call ..\cleancfast.bat
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_1 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_2 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_3 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_4 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_5 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_6 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_7 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_8 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_9 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_10 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_11 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_12 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_13 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_14 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_15 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_16 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_17 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_21 /V
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe FMSNL_22 /V
cd ..\
if %1==FM_SNL goto end

:NBS
echo NBS Tests MV100A, MV100O, MV100Z
cd NBS
call ..\cleancfast.bat
cd MV100A
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe MV100A /V
cd ..\
cd MV100O
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe MV100O /V
cd ..\
cd MV100Z
..\..\scripts\background -u 98 ..\..\..\bin\cfast.exe MV100Z /V
cd ..\..\
if %1==NBS goto end

:High_Bay
echo High Bay Tests
cd High_Bay
call ..\cleancfast.bat
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_01 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_02 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_03 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_04 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_05 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_06 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_07 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Hawaii_Test_11 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_01 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_02 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_03 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_04 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_05 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_06 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_07 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_09 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_10 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_11 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_12 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_13 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_14 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_15 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_17 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_18 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_19 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe USN_Iceland_Test_20 /V
cd ..\
if %1==High_Bay goto end

:Steckler
echo Steckler Compartment Tests
cd Steckler_Compartment
call ..\CleanCFAST
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_010 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_011 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_012 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_612 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_013 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_014 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_018 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_710 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_810 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_016 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_017 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_022 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_023 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_030 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_041 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_019 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_020 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_021 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_114 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_144 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_212 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_242 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_410 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_210 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_310 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_240 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_116 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_122 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_224 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_324 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_220 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_221 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_514 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_544 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_512 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_542 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_610 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_510 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_540 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_517 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_622 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_522 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_524 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_541 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_520 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_521 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_513 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_160 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_163 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_164 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_165 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_162 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_167 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_161 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe Steckler_166 /V
cd ..\
if %1==Steckler_Compartment goto end

:Dunes2000
echo NIST Dunes 2000 tests
cd NIST_Dunes_2000
call ..\CleanCFAST
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC02 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC05 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC07 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC10 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC33 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC35 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC38 /V
..\scripts\background -u 98 ..\..\bin\cfast.exe NIST_Dunes_2000_SDC39 /V
cd ..\
if %1==Dunes_2000 goto end

if %1==ALL echo NBS Plaza Hotel Test must be run separately
if %1==ALL goto end
:PLAZA
echo NBS Plaza Hotel test 7
cd Multi
call ..\cleancfast.bat
rem ..\scripts\background -u 98 ..\..\bin\cfast.exe Multi /V
cd ..\
if %1==PLAZA goto end

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
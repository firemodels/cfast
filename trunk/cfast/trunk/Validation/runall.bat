call cleanall.bat
echo.| time
echo NBS 1 room furniture tests 1, 6
cd 1rfurn
..\..\bin\cfast 1rfurn1 /V
..\..\bin\cfast 1rfurn6 /V
cd ..\
echo NBS 1 room wall burning test 1, 2
cd 1rwall
..\..\bin\cfast 1rwall1 /V
..\..\bin\cfast 1rwall2 /V
cd ..\
echo FM NBS 4 room tests 19, 21
cd fm_nbs
..\..\bin\cfast fm19 /V
..\..\bin\cfast fm21 /V
cd ..\
echo NBS Plaza Hotel test 7
cd multi
..\..\bin\cfast multi /V
cd ..\
echo VTT Cases 1, 2, 3
cd VTT\Case_1
..\..\..\bin\cfast VTT_C1 /V
cd ..\..
cd VTT\Case_2
..\..\..\bin\cfast VTT_C2 /V
cd ..\..
cd VTT\Case_3
..\..\..\bin\cfast VTT_C3 /V
cd ..\..
echo NIST_NRC tests 1-5, 7-10, 13-18
cd NIST_NRC\Test_1\
..\..\..\bin\cfast NIST_NRC_T1 /V
cd ..\..
cd NIST_NRC\Test_2\
..\..\..\bin\cfast NIST_NRC_T2 /V
cd ..\..
cd NIST_NRC\Test_3\
..\..\..\bin\cfast NIST_NRC_T3 /V
cd ..\..
cd NIST_NRC\Test_4\
..\..\..\bin\cfast NIST_NRC_T4 /V
cd ..\..
cd NIST_NRC\Test_5\
..\..\..\bin\cfast NIST_NRC_T5 /V
cd ..\..
cd NIST_NRC\Test_7\
..\..\..\bin\cfast NIST_NRC_T7 /V
cd ..\..
cd NIST_NRC\Test_8\
..\..\..\bin\cfast NIST_NRC_T8 /V
cd ..\..
cd NIST_NRC\Test_9\
..\..\..\bin\cfast NIST_NRC_T9 /V
cd ..\..
cd NIST_NRC\Test_10\
..\..\..\bin\cfast NIST_NRC_T10 /V
cd ..\..
cd NIST_NRC\Test_13\
..\..\..\bin\cfast NIST_NRC_T13 /V
cd ..\..
cd NIST_NRC\Test_14\
..\..\..\bin\cfast NIST_NRC_T14 /V
cd ..\..
cd NIST_NRC\Test_15\
..\..\..\bin\cfast NIST_NRC_T15 /V
cd ..\..
cd NIST_NRC\Test_16\
..\..\..\bin\cfast NIST_NRC_T16 /V
cd ..\..
cd NIST_NRC\Test_17\
..\..\..\bin\cfast NIST_NRC_T17 /V
cd ..\..
cd NIST_NRC\Test_18\
..\..\..\bin\cfast NIST_NRC_T18 /V
cd ..\..
echo iBMB_4 Test 1
cd iBMB_4\
..\..\bin\cfast iBMB_4_T1 /V
cd ..\
echo iBMB_5 Test 4
cd iBMB_5\
..\..\bin\cfast iBMB_5_T4 /V
cd ..\
echo FM SNL Tests
cd FM_SNL\Test_1
..\..\..\bin\cfast FMSNL_1 /V
cd ..\..\
cd FM_SNL\Test_3
..\..\..\bin\cfast FMSNL_3 /V
cd ..\..\
cd FM_SNL\Test_4
..\..\..\bin\cfast FMSNL_4 /V
cd ..\..\
cd FM_SNL\Test_5
..\..\..\bin\cfast FMSNL_5 /V
cd ..\..\
cd FM_SNL\Test_6
..\..\..\bin\cfast FMSNL_6 /V
cd ..\..\
cd FM_SNL\Test_7
..\..\..\bin\cfast FMSNL_7 /V
cd ..\..\
cd FM_SNL\Test_8
..\..\..\bin\cfast FMSNL_8 /V
cd ..\..\
cd FM_SNL\Test_9
..\..\..\bin\cfast FMSNL_9 /V
cd ..\..\
cd FM_SNL\Test_10
..\..\..\bin\cfast FMSNL_10 /V
cd ..\..\
cd FM_SNL\Test_11
..\..\..\bin\cfast FMSNL_11 /V
cd ..\..\
cd FM_SNL\Test_12
..\..\..\bin\cfast FMSNL_12 /V
cd ..\..\
cd FM_SNL\Test_13
..\..\..\bin\cfast FMSNL_13 /V
cd ..\..\
cd FM_SNL\Test_14
..\..\..\bin\cfast FMSNL_14 /V
cd ..\..\
cd FM_SNL\Test_15
..\..\..\bin\cfast FMSNL_15 /V
cd ..\..\
cd FM_SNL\Test_16
..\..\..\bin\cfast FMSNL_16 /V
cd ..\..\
cd FM_SNL\Test_17
..\..\..\bin\cfast FMSNL_17 /V
cd ..\..\
cd FM_SNL\Test_18
..\..\..\bin\cfast FMSNL_18 /V
cd ..\..\
cd FM_SNL\Test_19
..\..\..\bin\cfast FMSNL_19 /V
cd ..\..\
cd FM_SNL\Test_20
..\..\..\bin\cfast FMSNL_20 /V
cd ..\..\
cd FM_SNL\Test_21
..\..\..\bin\cfast FMSNL_21 /V
cd ..\..\
echo NBS Tests MV100A, MV100O, MV100Z
cd NBS\MV100A
..\..\..\bin\cfast MV100A /V
cd ..\..\
cd NBS\MV100O
..\..\..\bin\cfast MV100O /V
cd ..\..\
cd NBS\MV100Z
..\..\..\bin\cfast MV100Z /V
cd ..\..\
echo High Bay Tests
cd High_Bay
..\..\bin\cfast Keflavik_14 /V
..\..\bin\cfast Keflavik_15 /V
..\..\bin\cfast Keflavik_17 /V
..\..\bin\cfast Keflavik_18 /V
..\..\bin\cfast Keflavik_20 /V
..\..\bin\cfast Keflavik_21 /V
..\..\bin\cfast Keflavik_5 /V
..\..\bin\cfast Keflavik_6 /V
..\..\bin\cfast Keflavik_7 /V
cd ..\
echo.| time

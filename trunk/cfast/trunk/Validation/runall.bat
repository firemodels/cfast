echo off
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
cd fmnbs
..\..\bin\cfast fm19 /V
..\..\bin\cfast fm21 /V
cd ..\
echo NBS Plaza Hotel test 7
cd multi
..\..\bin\cfast multi /V
cd ..\
echo VTT Cases 1, 2, 3
cd "VTT\Case 1"
..\..\..\bin\cfast "VTT C1" /V
cd ..\..
cd "VTT\Case 2"
..\..\..\bin\cfast "VTT C2" /V
cd ..\..
cd "VTT\Case 3"
..\..\..\bin\cfast "VTT C3" /V
cd ..\..
echo NIST_NRC tests 1-5, 7-10, 13-18
cd "NIST_NRC\Test 1\"
..\..\..\bin\cfast "NIST_NRC T1" /V
cd ..\..
cd "NIST_NRC\Test 2\"
..\..\..\bin\cfast "NIST_NRC T2" /V
cd ..\..
cd "NIST_NRC\Test 3\"
..\..\..\bin\cfast "NIST_NRC T3" /V
cd ..\..
cd "NIST_NRC\Test 4\"
..\..\..\bin\cfast "NIST_NRC T4" /V
cd ..\..
cd "NIST_NRC\Test 5\"
..\..\..\bin\cfast "NIST_NRC T5" /V
cd ..\..
cd "NIST_NRC\Test 7\"
..\..\..\bin\cfast "NIST_NRC T7" /V
cd ..\..
cd "NIST_NRC\Test 8\"
..\..\..\bin\cfast "NIST_NRC T8" /V
cd ..\..
cd "NIST_NRC\Test 9\"
..\..\..\bin\cfast "NIST_NRC T9" /V
cd ..\..
cd "NIST_NRC\Test 10\"
..\..\..\bin\cfast "NIST_NRC T10" /V
cd ..\..
cd "NIST_NRC\Test 13\"
..\..\..\bin\cfast "NIST_NRC T13" /V
cd ..\..
cd "NIST_NRC\Test 14\"
..\..\..\bin\cfast "NIST_NRC T14" /V
cd ..\..
cd "NIST_NRC\Test 15\"
..\..\..\bin\cfast "NIST_NRC T15" /V
cd ..\..
cd "NIST_NRC\Test 16\"
..\..\..\bin\cfast "NIST_NRC T16" /V
cd ..\..
cd "NIST_NRC\Test 17\"
..\..\..\bin\cfast "NIST_NRC T17" /V
cd ..\..
cd "NIST_NRC\Test 18\"
..\..\..\bin\cfast "NIST_NRC T18" /V
cd ..\..
echo iBMB_4 Test 1
cd "iBMB_4\"
..\..\bin\cfast "iBMB_4 T1" /V
cd ..\
echo iBMB_5 Test 4
cd "iBMB_5\"
..\..\bin\cfast "iBMB_5 T4" /V
cd ..\
echo FM SNL Tests 4, 5, 21
cd "FMSNL\Test 4"
..\..\..\bin\cfast "FMSNL 4" /V
cd ..\..\
cd "FMSNL\Test 5"
..\..\..\bin\cfast "FMSNL 5" /V
cd ..\..\
cd "FMSNL\Test 21"
..\..\..\bin\cfast "FMSNL 21" /V
cd ..\..\
echo NBS Tests MV100A, MV100O, MV100Z
cd "NBS\MV100A"
..\..\..\bin\cfast "NBS MV100A" /V
cd ..\..\
cd "NBS\MV100O"
..\..\..\bin\cfast "NBS MV100O" /V
cd ..\..\
cd "NBS\MV100Z"
..\..\..\bin\cfast "NBS MV100Z" /V
cd ..\..\
echo.| time

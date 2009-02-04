echo off
call cleanall.bat
echo NBS 1 room furniture tests 1, 6
cd 1rfurn
..\..\bin\cfast 1rfurn1
..\..\bin\cfast 1rfurn6
cd ..\
echo NBS 1 room wall burning test 1, 2
cd 1rwall
..\..\bin\cfast 1rwall1
..\..\bin\cfast 1rwall2
cd ..\
echo FM NBS 4 room tests 19, 21
cd fmnbs
..\..\bin\cfast fm19
..\..\bin\cfast fm21
cd ..\
echo NBS Plaza Hotel test 7
cd multi
..\..\bin\cfast multi
cd ..\
echo BE2 Cases 1, 2, 3
cd "BE2\Case 1"
..\..\..\bin\cfast "BE2 C1"
cd ..\..
cd "BE2\Case 2"
..\..\..\bin\cfast "BE2 C2"
cd ..\..
cd "BE2\Case 3"
..\..\..\bin\cfast "BE2 C3"
cd ..\..
echo BE3 tests 1-5, 7-10, 13-18
cd "BE3\Test 1\"
..\..\..\bin\cfast "BE3 T1"
cd ..\..
cd "BE3\Test 2\"
..\..\..\bin\cfast "BE3 T2"
cd ..\..
cd "BE3\Test 3\"
..\..\..\bin\cfast "BE3 T3"
cd ..\..
cd "BE3\Test 4\"
..\..\..\bin\cfast "BE3 T4"
cd ..\..
cd "BE3\Test 5\"
..\..\..\bin\cfast "BE3 T5"
cd ..\..
cd "BE3\Test 7\"
..\..\..\bin\cfast "BE3 T7"
cd ..\..
cd "BE3\Test 8\"
..\..\..\bin\cfast "BE3 T8"
cd ..\..
cd "BE3\Test 9\"
..\..\..\bin\cfast "BE3 T9"
cd ..\..
cd "BE3\Test 10\"
..\..\..\bin\cfast "BE3 T10"
cd ..\..
cd "BE3\Test 13\"
..\..\..\bin\cfast "BE3 T13"
cd ..\..
cd "BE3\Test 14\"
..\..\..\bin\cfast "BE3 T14"
cd ..\..
cd "BE3\Test 15\"
..\..\..\bin\cfast "BE3 T15"
cd ..\..
cd "BE3\Test 16\"
..\..\..\bin\cfast "BE3 T16"
cd ..\..
cd "BE3\Test 17\"
..\..\..\bin\cfast "BE3 T17"
cd ..\..
cd "BE3\Test 18\"
..\..\..\bin\cfast "BE3 T18"
cd ..\..
echo BE4 Test 1
cd "BE4\"
..\..\bin\cfast "BE4 T1"
cd ..\
echo BE5 Test 4
cd "BE5\"
..\..\bin\cfast "BE5 T4"
cd ..\
echo FM SNL Tests 4, 5, 21
cd "FMSNL\Test 4"
..\..\..\bin\cfast "FMSNL 4"
cd ..\..\
cd "FMSNL\Test 5"
..\..\..\bin\cfast "FMSNL 5"
cd ..\..\
cd "FMSNL\Test 21"
..\..\..\bin\cfast "FMSNL 21"
cd ..\..\
echo NBS Tests MV100A, MV100O, MV100Z
cd "NBS\MV100A"
..\..\..\bin\cfast "NBS MV100A"
cd ..\..\
cd "NBS\MV100O"
..\..\..\bin\cfast "NBS MV100O"
cd ..\..\
cd "NBS\MV100Z"
..\..\..\bin\cfast "NBS MV100Z"
cd ..\..\

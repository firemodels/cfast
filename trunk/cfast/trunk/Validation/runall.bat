echo off
call cleanall.bat
echo NBS 1 room furniture tests 1, 6
cd 1rfurn
..\..\bin\cfast 1rfurn1 /G
..\..\bin\cfast 1rfurn6 /G
cd ..\
echo NBS 1 room wall burning test 1, 2
cd 1rwall
..\..\bin\cfast 1rwall1 /G
..\..\bin\cfast 1rwall2 /G
cd ..\
echo FM NBS 4 room tests 19, 21
cd fmnbs
..\..\bin\cfast fm19 /G
..\..\bin\cfast fm21 /G
cd ..\
echo NBS Plaza Hotel test 7
cd multi
..\..\bin\cfast multi /G
cd ..\
echo BE2 Cases 1, 2, 3
cd "BE2\Case 1"
..\..\..\bin\cfast "BE2 C1" /G
cd ..\..
cd "BE2\Case 2"
..\..\..\bin\cfast "BE2 C2" /G
cd ..\..
cd "BE2\Case 3"
..\..\..\bin\cfast "BE2 C3" /G
cd ..\..
echo BE3 tests 1-5, 7-10, 13-18
cd "BE3\Test 1\"
..\..\..\bin\cfast "BE3 T1" /G
cd ..\..
cd "BE3\Test 2\"
..\..\..\bin\cfast "BE3 T2" /G
cd ..\..
cd "BE3\Test 3\"
..\..\..\bin\cfast "BE3 T3" /G
cd ..\..
cd "BE3\Test 4\"
..\..\..\bin\cfast "BE3 T4" /G
cd ..\..
cd "BE3\Test 5\"
..\..\..\bin\cfast "BE3 T5" /G
cd ..\..
cd "BE3\Test 7\"
..\..\..\bin\cfast "BE3 T7" /G
cd ..\..
cd "BE3\Test 8\"
..\..\..\bin\cfast "BE3 T8" /G
cd ..\..
cd "BE3\Test 9\"
..\..\..\bin\cfast "BE3 T9" /G
cd ..\..
cd "BE3\Test 10\"
..\..\..\bin\cfast "BE3 T10" /G
cd ..\..
cd "BE3\Test 13\"
..\..\..\bin\cfast "BE3 T13" /G
cd ..\..
cd "BE3\Test 14\"
..\..\..\bin\cfast "BE3 T14" /G
cd ..\..
cd "BE3\Test 15\"
..\..\..\bin\cfast "BE3 T15" /G
cd ..\..
cd "BE3\Test 16\"
..\..\..\bin\cfast "BE3 T16" /G
cd ..\..
cd "BE3\Test 17\"
..\..\..\bin\cfast "BE3 T17" /G
cd ..\..
cd "BE3\Test 18\"
..\..\..\bin\cfast "BE3 T18" /G
cd ..\..
echo BE4 Test 1
cd "BE4\"
..\..\bin\cfast "BE4 T1" /G
cd ..\
echo BE5 Test 4
cd "BE5\"
..\..\bin\cfast "BE5 T4" /G
cd ..\
echo FM SNL Tests 4, 5, 21
cd "FMSNL\Test 4"
..\..\..\bin\cfast "FMSNL 4" /G
cd ..\..\
cd "FMSNL\Test 5"
..\..\..\bin\cfast "FMSNL 5" /G
cd ..\..\
cd "FMSNL\Test 21"
..\..\..\bin\cfast "FMSNL 21" /G
cd ..\..\
echo NBS Tests MV100A, MV100O, MV100Z
cd "NBS\MV100A"
..\..\..\bin\cfast "NBS MV100A" /G
cd ..\..\
cd "NBS\MV100O"
..\..\..\bin\cfast "NBS MV100O" /G
cd ..\..\
cd "NBS\MV100Z"
..\..\..\bin\cfast "NBS MV100Z" /G
cd ..\..\

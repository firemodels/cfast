@echo off
echo Make Technical Reference Guide
cd ..\Manuals\Tech_Ref
call make_guide.bat

echo Make Users Guide
cd ..\Users_Guide
call make_guide.bat

echo Make Validation Guide
cd ..\Validation_Guide
call make_guide.bat

echo Make Configuration Guide
cd ..\Configuration_Guide
call make_guide.bat

cd ..\..\Utilities


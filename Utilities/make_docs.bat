@echo off
echo Make Technical Reference Guide
cd ..\Docs\Tech_Ref
make_guide.bat

echo Make Users Guide
cd ..\Users_Guide
make_guide.bat

echo Make Validation Guide
cd ..\Validation_Guide
make_guide.bat

echo Make Configuration Guide
cd ..\Configuration_Guide
make_guide.bat

cd ..\..\Utilities


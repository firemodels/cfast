@echo off

@echo *** Copying Executables
copy ..\Bin\CFAST.exe c:\Users\rpeacoc\cfastgitclean\bin /Y
copy ..\Bin\CEdit.exe c:\Users\rpeacoc\cfastgitclean\bin /Y
copy ..\Bin\Smokeview.exe c:\Users\rpeacoc\cfastgitclean\bin /Y
copy ..\Bin\Smokediff.exe c:\Users\rpeacoc\cfastgitclean\bin /Y

@echo *** copying DLLs
copy ..\bin\C1*.dll c:\Users\rpeacoc\cfastgitclean\bin /Y
copy ..\bin\pthreadVC2_x64.dll c:\Users\rpeacoc\cfastgitclean\bin /Y
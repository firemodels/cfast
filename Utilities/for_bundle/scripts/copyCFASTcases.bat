@echo off

call %cfastrepo%\Utilities\scripts\getopts.bat %*

set in=%infile%.in

if not exist %outdir%\%dir% mkdir %outdir%\%dir%

copy %dir%\%in% %outdir%\%dir%\%in% > Nul
if EXIST %outdir%\%dir%\%in%     echo %dir%\%in% copied
if NOT exist %outdir%\%dir%\%in% echo ***error: %dir%\%in% failed to copy
if exist %dir%\%infile%.ini copy %dir%\%infile%.ini %outdir%\%dir%\%infile%.ini > Nul
if exist %dir%\%infile%.ssf copy %dir%\%infile%.ssf %outdir%\%dir%\%infile%.ssf > Nul

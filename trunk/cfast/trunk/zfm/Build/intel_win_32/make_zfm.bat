set intelbin="%IFORT_COMPILER12%\bin"

call %intelbin%\ifortvars ia32
erase *.obj
make -f ..\makefile intel_win_32
pause
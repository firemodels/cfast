# Makefile for CFAST, Version 6

# To use this makefile, select the appropriate compiler and OS from the list below. For example, to
# compile the 32 bit version of CFAST under Linux using the Intel compilers type:
#
# make intel_linux_32

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

# The variable VPATH defines where the source code is relative to the current directory

VPATH = Source
INCLUDE = ../Include

# Definition of the object variables

objf = datamodules.o cfast.o auxiliary.o initialization.o input.o numerics.o \
              output.o outputsmv.o outputspreadsheet.o ssHeaders.o \
              conduction.o cyl_conduct.o convection.o radiation.o target.o \
              fire.o flowfan.o flowhall.o flowhorizontal.o flowvertical.o 

objwin = $(objf:.o=.obj)

# General Purpose Rules

no_target:
	@echo \******** You did not specify a make target \********
	@echo Please read the comments at the top of the makefile

setup:
%.o : %.mod

setup_win:
%.obj : %.mod

.SUFFIXES: .f90 .f .o .obj

.f.o:
	$(FCOMPL) -c $(FFLAGS) $<
.f.obj:
	$(FCOMPL) -c $(FFLAGS) $<
.f90.o:
	$(FCOMPL) -c $(FFLAGS) $<
.f90.obj:
	$(FCOMPL) -c $(FFLAGS) $<

# run make_fds.sh or make_fds.bat scripts in individual build directories

# ------------- Windows -------------------

# 32 bit
intel_win_32_db : FFLAGS = -fpp -D pp_ibmpc /Qvec_report0  /Qsave /Qzero /nologo /debug:full /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f95
intel_win_32_db : FCOMPL = ifort
intel_win_32_db : obj = cfast6_win_32_db
intel_win_32_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

intel_win_32 : FFLAGS = -O2 -fpp -D pp_ibmpc /Qvec_report0 /Qsave /Qzero
intel_win_32 : FCOMPL = ifort
intel_win_32 : obj = cfast6_win_32
intel_win_32 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

# 64 bit
intel_win_64 : FFLAGS = -O2 -fpp -D pp_ibmpc /Qvec_report0  /Qsave /Qzero
intel_win_64 : FCOMPL = ifort
intel_win_64 : obj = cfast6_win_64
intel_win_64 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

intel_win_64_db : FFLAGS = -fpp -D pp_ibmpc /Qvec_report0  /Qsave /Qzero /nologo /debug:full /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f95
intel_win_64_db : FCOMPL = ifort
intel_win_64_db : obj = cfast6_win_64_db
intel_win_64_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin)  	

# ------------- Linux -------------------

# 32 bit
intel_linux_32 : FFLAGS = -fpp -m32 -O3 -save -zero -static -vec_report0 -I $(INCLUDE)
intel_linux_32 : FCOMPL = ifort
intel_linux_32 : obj = cfast6_linux_32
intel_linux_32 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

intel_linux_32_db : FFLAGS = -m32 -check all -warn unused -O0 -save -zero -auto -WB -traceback -g -fpe0 -fltconsistency -static -vec_report0 -ftrapuv -debug full
intel_linux_32_db : FCOMPL = ifort
intel_linux_32_db : obj = cfast6_linux_32_db
intel_linux_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 64 bit
intel_linux_64 : FFLAGS = -fpp -m64 -O3 -save -zero -static -vec_report0 -I $(INCLUDE)
intel_linux_64 : FCOMPL = ifort
intel_linux_64 : obj = cfast6_linux_64
intel_linux_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

intel_linux_64_db : FFLAGS = -m64 -check -warn unused -O0 -save -zero -auto -WB -traceback -g -fpe0 -fltconsistency -static -vec_report0
intel_linux_64_db : FCOMPL = ifort
intel_linux_64_db : obj = cfast6_linux_64_db
intel_linux_64_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# ------------- OSX -------------------

# 32 bit
intel_osx_32 : FFLAGS = -fpp -O3 -save -zero  -m32 -heap-arrays -static-intel -vec_report0 -I $(INCLUDE)
intel_osx_32 : FCOMPL = ifort
intel_osx_32 : obj = cfast6_osx_32
intel_osx_32 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_32_db : FFLAGS = -check -warn unused -O0 -save -zero -m32 -g -traceback -fpe:0 -fltconsistency -FR -WB -vec_report0
intel_osx_32_db : FCOMPL = ifort
intel_osx_32_db : obj = cfast6_osx_32_db
intel_osx_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

# 64 bit
intel_osx_64 : FFLAGS = -fpp -O3 -save -zero -m64 -heap-arrays -static-intel -I $(INCLUDE)
intel_osx_64 : FCOMPL = ifort
intel_osx_64 : obj = cfast6_osx_64
intel_osx_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_64_db : FFLAGS = -check -warn unused -O0 -save -zero -m64 -g -traceback -save-temps -fpe:0 -fltconsistency -fp-stack-check -FR -WB -vec_report0
intel_osx_64_db : FCOMPL = ifort
intel_osx_64_db : obj = cfast6_osx_64_db
intel_osx_64_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

#*** End Compiler Specific Rules ***

# *** Object Dependencies ***


cfast.o : datamodules.o auxiliary.o initialization.o input.o numerics.o \
              output.o outputsmv.o outputspreadsheet.o ssHeaders.o \
              conduction.o cyl_conduct.o convection.o radiation.o target.o \
              fire.o flowfan.o flowhall.o flowhorizontal.o flowvertical.o


cfast.obj : datamodules.obj auxiliary.obj initialization.obj input.obj numerics.obj \
              output.obj outputsmv.obj outputspreadsheet.obj ssHeaders.obj \
              conduction.obj cyl_conduct.obj convection.obj radiation.obj target.obj \
              fire.obj flowfan.obj flowhall.obj flowhorizontal.obj flowvertical.obj

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


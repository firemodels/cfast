# Makefile for CFAST, Version 7

# To use this makefile, select the appropriate compiler and OS from the list below. For example, to
# compile the 32 bit version of CFAST under Linux using the Intel compilers type:
#
# make intel_linux_32

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

# The variable VPATH defines where the source code is relative to the current directory

VPATH = Source

ifeq ($(shell echo "check_quotes"),"check_quotes")
# windows
  GIT_HASH := $(shell ..\scripts\githash)
  GIT_DATE := $(shell ..\scripts\gitlog)
  BUILD_DATE := $(shell date /t) $(shell time /t)
else
# linux/osx
  GIT_HASH := $(shell git describe --long --dirty)
  GIT_DATE := $(shell git log -1 --format=%cd)
  BUILD_DATE := $(shell date "+%b %d, %Y  %T")
endif

GITINFO_BASE=-DGITHASH_PP=\"$(GIT_HASH)\" -DGITDATE_PP=\""$(GIT_DATE)\""
GITINFO=-fpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
GITINFOGNU=-cpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""



# Definition of the object variables

objf = prec.o srev.o cfast_parameters.o cfast_structures.o cfast_data.o cfast_interfaces.o keyboard_int.o convection.o conduction.o \
       cyl_conduct.o utilities.o numerics.o flowvertical.o flowfan.o fire.o ssHeaders.o outputspreadsheet.o flowhorizontal.o radiation.o \
       outputsmv.o target.o output.o solve.o initialization.o input.o cfast.o 

objwin = $(objf:.o=.obj)

objfgnu = prec.o srev.o cfast_parameters.o cfast_structures.o cfast_data.o cfast_interfaces.o keyboard_gnu.o convection.o conduction.o \
       cyl_conduct.o utilities.o numerics.o flowvertical.o flowfan.o fire.o ssHeaders.o outputspreadsheet.o flowhorizontal.o radiation.o \
       outputsmv.o target.o output.o solve.o initialization.o input.o cfast.o  


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

# gnu windows
gnu_win_64 : FFLAGS = -O2 $(GITINFO)
gnu_win_64 : FCOMPL = gfortran
gnu_win_64 : LFLAGS = 
gnu_win_64 : obj = cfast7_win_64
gnu_win_64 : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

# 32 bit
intel_win_32_db : FFLAGS = /Qsave /Qinit:zero /debug:full /Z7 /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f95 $(GITINFO)
intel_win_32_db : FCOMPL = ifort
intel_win_32_db : obj = cfast7_win_32_db
intel_win_32_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

intel_win_32 : FFLAGS = /O2 /Qsave /Qinit:zero /fp:strict /fp:except /traceback /stand:f08  $(GITINFO)
intel_win_32 : FCOMPL = ifort
intel_win_32 : obj = cfast7_win_32
intel_win_32 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

# 64 bit
intel_win_64 : FFLAGS = /O2 /Qsave /Qinit:zero /fp:strict /fp:except /traceback /stand:f08  $(GITINFO)
intel_win_64 : FCOMPL = ifort
intel_win_64 : obj = cfast7_win_64
intel_win_64 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

intel_win_64_db : FFLAGS = /Qsave /Qinit:zero /debug:full /Z7 /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f08 $(GITINFO)
intel_win_64_db : FCOMPL = ifort
intel_win_64_db : obj = cfast7_win_64_db
intel_win_64_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin)  	

# ------------- Linux -------------------

# 32 bit
intel_linux_32 : FFLAGS = -fpp -m32 -O3 -save -zero -static -vec_report:0 $(GITINFO)
intel_linux_32 : FCOMPL = ifort
intel_linux_32 : obj = cfast7_linux_32
intel_linux_32 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

intel_linux_32_db : FFLAGS = -m32 -check all -ftrapuv -warn unused -gen-interfaces -warn interfaces -O0 -WB -traceback -g -fpe0 -fltconsistency -static -vec_report:0 -debug full $(GITINFO)
intel_linux_32_db : FCOMPL = ifort
intel_linux_32_db : obj = cfast7_linux_32_db
intel_linux_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# gnu osx
gnu_osx : FFLAGS = -O2 $(GITINFOGNU)
gnu_osx : FCOMPL = gfortran
gnu_osx : LFLAGS = 
gnu_osx : obj = cfast7_gnu_osx
gnu_osx : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

# gnu linux
gnu_linux : FFLAGS = -O2 $(GITINFO)
gnu_linux : FCOMPL = gfortran
gnu_linux : LFLAGS = 
gnu_linux : obj = cfast7_gnu_linux
gnu_linux : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

# 64 bit
intel_linux_64 : FFLAGS = -m64 -O3 -gen-interfaces -warn interfaces -traceback $(GITINFO)
intel_linux_64 : LFLAGS = -static-intel
intel_linux_64 : FCOMPL = ifort
intel_linux_64 : obj = cfast7_linux_64
intel_linux_64 : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

#intel_linux_64_db : FFLAGS = -m64 -check all -ftrapuv -warn unused -gen-interfaces -warn interfaces -O0 -WB -traceback -g -fpe0 -fltconsistency -debug full
intel_linux_64_db : FFLAGS = -m64 -check -warn unused -O0 -auto -WB -traceback -g -fpe0 -fltconsistency -stand:f08 $(GITINFO)
intel_linux_64_db : LFLAGS = -static-intel
intel_linux_64_db : FCOMPL = ifort
intel_linux_64_db : obj = cfast7_linux_64_db
intel_linux_64_db : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

# ------------- OSX -------------------

# 32 bit
intel_osx_32 : FFLAGS = -fpp -O3 -save -zero  -m32 -heap-arrays -static-intel -vec_report:0 $(GITINFO)
intel_osx_32 : FCOMPL = ifort
intel_osx_32 : obj = cfast7_osx_32
intel_osx_32 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_32_db : FFLAGS = -check -warn unused -O0 -save -zero -m32 -g -traceback -fpe:0 -fltconsistency -FR -WB -vec_report:0 $(GITINFO)
intel_osx_32_db : FCOMPL = ifort
intel_osx_32_db : obj = cfast7_osx_32_db
intel_osx_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

# 64 bit
intel_osx_64 : FFLAGS = -fpp -O3 -save -zero -m64 -heap-arrays -static-intel $(GITINFO)
intel_osx_64 : FCOMPL = ifort
intel_osx_64 : obj = cfast7_osx_64
intel_osx_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_64_db : FFLAGS = -check -warn unused -O0 -save -zero -m64 -g -traceback -save-temps -fpe:0 -fltconsistency -fp-stack-check -FR -WB -vec_report:0 $(GITINFO)
intel_osx_64_db : FCOMPL = ifort
intel_osx_64_db : obj = cfast7_osx_64_db
intel_osx_64_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

#*** End Compiler Specific Rules ***

# *** Object Dependencies ***


cfast.o : utilities.o initialization.o input.o numerics.o \
              output.o outputsmv.o outputspreadsheet.o ssHeaders.o \
              conduction.o cyl_conduct.o convection.o radiation.o target.o \
              fire.o flowfan.o flowhorizontal.o flowvertical.o solve.o


cfast.obj : utilities.obj initialization.obj input.obj numerics.obj \
              output.obj outputsmv.obj outputspreadsheet.obj ssHeaders.obj \
              conduction.obj cyl_conduct.obj convection.obj radiation.obj target.obj \
              fire.obj flowfan.obj flowhorizontal.obj flowvertical.obj solve.o

#-------------- force compilation of srev.c -----------

srev.o:	.FORCE
srev.obj:	.FORCE

.FORCE:

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


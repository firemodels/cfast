# Makefile for CFAST, Version 7

# To use this makefile, select the appropriate compiler and OS from the list below. For example, to
# compile the 64 bit version of CFAST under Linux using the Intel compilers type:
#
# make intel_linux

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

# The variable VPATH defines where the source code is relative to the current directory

VPATH = ../../../Source/CFAST

ifeq ($(shell echo "check_quotes"),"check_quotes")
# windows
  GIT_HASH := $(shell ..\..\scripts\githash)
  GIT_DATE := $(shell ..\..\scripts\gitlog)
  BUILD_DATE := $(shell date /t) $(shell time /t)
  GIT_BRANCH := $(shell ..\..\scripts\gitbranch)
else
# linux/osx
  GIT_HASH := $(shell git describe --long --dirty)
  GIT_DATE := $(shell git log -1 --format=%cd)
  BUILD_DATE := $(shell date "+%b %d, %Y  %T")
  GIT_BRANCH := $(shell git rev-parse --abbrev-ref HEAD)
endif

GITINFO_BASE=-DGITHASH_PP=\"$(GIT_HASH)-$(GIT_BRANCH)\" -DGITDATE_PP=\""$(GIT_DATE)\""
GITINFO=-fpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""
GITINFOGNU=-cpp $(GITINFO_BASE) -DBUILDDATE_PP=\""$(BUILD_DATE)\""

# Definition of the object variables

objf = prec.o cfast_parameters.o cfast_structures.o cfast_data.o convection.o keyboard_int.o \
       srev.o cfastexit.o utilities.o ssHeaders.o outputdebug.o compartments.o conduction.o flowfan.o flowhorizontal.o flowvertical.o fire.o \
       numerics.o radiation.o target.o outputsmv.o outputspreadsheet.o output.o solve.o initialization.o input_namelist.o input.o \
       cfast.o 

objwin = $(objf:.o=.obj)

objfgnu = prec.o cfast_parameters.o cfast_structures.o cfast_data.o convection.o keyboard_gnu.o \
       srev.o cfastexit.o utilities.o ssHeaders.o outputdebug.o compartments.o conduction.o flowfan.o flowhorizontal.o flowvertical.o fire.o \
       numerics.o radiation.o target.o outputsmv.o outputspreadsheet.o output.o solve.o initialization.o  input_namelist.o input.o cfast.o  


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

# ------------- Windows -------------------

gnu_win : FFLAGS = -O0 -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_win : FCOMPL = gfortran
gnu_win : obj = cfast7_win
gnu_win : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

gnu_win_db : FFLAGS = -O0 -Og -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_win_db : FCOMPL = gfortran
gnu_win_db : obj = cfast7_win_db
gnu_win_db : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

intel_win : FFLAGS = /O2 /fp:consistent /fpconstant /traceback $(GITINFO)
intel_win : FCOMPL = ifx
intel_win : obj = cfast7_win
intel_win : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

intel_win_db : FFLAGS = /Qtrapuv /Qinit:snan /debug:full /Z7 /Od /warn:all /warn:nointerfaces /Qtrapuv /fp:consistent /fpconstant /traceback /check:all /stand:f23 $(GITINFO)
intel_win_db : FCOMPL = ifx
intel_win_db : obj = cfast7_win_db
intel_win_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin)  	

# ------------- Linux -------------------
 
gnu_linux : FFLAGS = -O0 $(GITINFOGNU)
gnu_linux : FCOMPL = gfortran
gnu_linux : obj = cfast7_linux
gnu_linux : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

gnu_linux_db : FFLAGS = -O0 -std=f2018 -ggdb -finit-real=snan -finit-integer=-999999 -Wall -Werror -Wunused-parameter -Wno-target-lifetime -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -frecursive -ffpe-summary=none -fall-intrinsics -fbounds-check $(GITINFOGNU)
gnu_linux_db : FCOMPL = gfortran
gnu_linux_db : obj = cfast7_linux_db
gnu_linux_db : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

intel_linux : FFLAGS = -O2 -traceback $(GITINFO)
intel_linux : FCOMPL = ifx
intel_linux : obj = cfast7_linux
intel_linux : $(objf)
	$(FCOMPL) -o $(obj) $(objf)

intel_linux_db : FFLAGS = -check all -warn all -O0 -init=snan -traceback -g -fpe0 -fp-model strict -stand f2023 $(GITINFO)
intel_linux_db : FCOMPL = ifx
intel_linux_db : obj = cfast7_linux_db
intel_linux_db : $(objf)
	$(FCOMPL) -o $(obj) $(objf)

# ------------- OSX -------------------

gnu_osx : FFLAGS = -O0 -finit-local-zero -ffpe-trap=invalid,zero,overflow -ffpe-summary=none -fbacktrace $(GITINFOGNU)
gnu_osx: FCOMPL = gfortran
gnu_osx : obj = cfast7_osx
gnu_osx : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

gnu_osx_db : FFLAGS = -O0 -std=f2018 -ggdb -finit-real=snan -finit-integer=-999999 -Wall -Werror -Wunused-parameter -Wno-target-lifetime -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -frecursive -ffpe-summary=none -fall-intrinsics -fbounds-check $(GITINFOGNU)
gnu_osx_db : FCOMPL = gfortran
gnu_osx_db : obj = cfast7_osx_db
gnu_osx_db : $(objfgnu)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objfgnu)

intel_osx : FFLAGS = -O2 -traceback $(GITINFO)
intel_osx : FCOMPL = ifx
intel_osx : obj = cfast7_osx
intel_osx : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.7 -o $(obj) $(objf)

intel_osx_db : FFLAGS = -fpp -check all -warn all -O0 -g -traceback -fpe:0 -fp-model strict -fp-stack-check -FR $(GITINFO)
intel_osx_db : FCOMPL = ifx
intel_osx_db : obj = cfast7_osx_db
intel_osx_db : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.7 -o $(obj) $(objf)

# *** Object Dependencies ***

cfast.o : utilities.o cfastexit.o initialization.o input_namelist.o input.o numerics.o \
              output.o outputsmv.o outputspreadsheet.o ssHeaders.o \
              compartments.o conduction.o convection.o radiation.o target.o \
              fire.o flowfan.o flowhorizontal.o flowvertical.o solve.o


cfast.obj : utilities.obj cfastexit.obj initialization.obj input_namelist.o input.obj numerics.obj \
              output.obj outputsmv.obj outputspreadsheet.obj ssHeaders.obj \
              compartments.obj conduction.obj convection.obj radiation.obj target.obj \
              fire.obj flowfan.obj flowhorizontal.obj flowvertical.obj solve.o

#-------------- force compilation of srev.90 -----------

srev.o:	.FORCE
srev.obj:	.FORCE

.FORCE:

output.o:	.FORCE2
output.obj:	.FORCE2

.FORCE2:

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 

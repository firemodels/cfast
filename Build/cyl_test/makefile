# Makefile for cyl_test

VPATH = ../../../Source/cyl_test;../../../Source/CFAST

# Definition of the object variables

objf = prec.o cfast_parameters.o cyl_conduct.o cyl_test.o 

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

# ------------- Windows -------------------

# 64 bit
intel_win : FFLAGS = -O2 /Qopt-report:0 /Qopt-report-phase:vec  /Qsave /Qinit:zero /traceback /stand:f08 
intel_win : FCOMPL = ifort
intel_win : obj = cyltest_win
intel_win : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

intel_win_db : FFLAGS = /Qopt-report:0 /Qsave /nologo /debug:full /Z7 /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f08
intel_win_db : FCOMPL = ifort
intel_win_db : obj = cyltest_win_db
intel_win_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin)  	

# ------------- Linux -------------------

# 64 bit
intel_linux : FFLAGS = -m64 -O3 -gen-interfaces -warn interfaces -traceback
intel_linux : LFLAGS = -static-intel
intel_linux : FCOMPL = ifort
intel_linux : obj = cyltest_linux
intel_linux : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

#intel_linux_db : FFLAGS = -m64 -check all -ftrapuv -warn unused -gen-interfaces -warn interfaces -O0 -WB -traceback -g -fpe0 -fltconsistency -debug full
intel_linux_db : FFLAGS = -m64 -check -warn unused -O0 -auto -WB -traceback -g -fpe0 -fltconsistency -stand:f08
intel_linux_db : LFLAGS = -static-intel
intel_linux_db : FCOMPL = ifort
intel_linux_db : obj = cyltest_linux_db
intel_linux_db : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

# ------------- OSX -------------------

# 64 bit
intel_osx : FFLAGS = -fpp -O3 -save -zero -m64 -heap-arrays -static-intel
intel_osx : FCOMPL = ifort
intel_osx : obj = cyltest_osx
intel_osx : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_db : FFLAGS = -check -warn unused -O0 -save -zero -m64 -g -traceback -save-temps -fpe:0 -fltconsistency -fp-stack-check -FR -WB -vec_report:0
intel_osx_db : FCOMPL = ifort
intel_osx_db : obj = cyltest_osx_db
intel_osx_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

#*** End Compiler Specific Rules ***


#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


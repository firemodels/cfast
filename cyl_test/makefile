# Makefile for cyltest

VPATH = .;../CFAST/Source

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
intel_win_64 : FFLAGS = -O2 /Qopt-report:0 /Qopt-report-phase:vec  /Qsave /Qinit:zero /traceback /stand:f08 
intel_win_64 : FCOMPL = ifort
intel_win_64 : obj = cyltest_win_64
intel_win_64 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

intel_win_64_db : FFLAGS = /Qopt-report:0 /Qsave /nologo /debug:full /Z7 /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f08
intel_win_64_db : FCOMPL = ifort
intel_win_64_db : obj = cyltest_win_64_db
intel_win_64_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin)  	

# ------------- Linux -------------------

# 64 bit
intel_linux_64 : FFLAGS = -m64 -O3 -gen-interfaces -warn interfaces -traceback
intel_linux_64 : LFLAGS = -static-intel
intel_linux_64 : FCOMPL = ifort
intel_linux_64 : obj = cyltest_linux_64
intel_linux_64 : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

#intel_linux_64_db : FFLAGS = -m64 -check all -ftrapuv -warn unused -gen-interfaces -warn interfaces -O0 -WB -traceback -g -fpe0 -fltconsistency -debug full
intel_linux_64_db : FFLAGS = -m64 -check -warn unused -O0 -auto -WB -traceback -g -fpe0 -fltconsistency -stand:f08
intel_linux_64_db : LFLAGS = -static-intel
intel_linux_64_db : FCOMPL = ifort
intel_linux_64_db : obj = cyltest_linux_64_db
intel_linux_64_db : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

# ------------- OSX -------------------

# 64 bit
intel_osx_64 : FFLAGS = -fpp -O3 -save -zero -m64 -heap-arrays -static-intel
intel_osx_64 : FCOMPL = ifort
intel_osx_64 : obj = cyltest_osx_64
intel_osx_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_64_db : FFLAGS = -check -warn unused -O0 -save -zero -m64 -g -traceback -save-temps -fpe:0 -fltconsistency -fp-stack-check -FR -WB -vec_report:0
intel_osx_64_db : FCOMPL = ifort
intel_osx_64_db : obj = cyltest_osx_64_db
intel_osx_64_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

#*** End Compiler Specific Rules ***


#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


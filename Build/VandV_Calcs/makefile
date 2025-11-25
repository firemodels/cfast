# Makefile for VandV_Calcs

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

# The variable VPATH defines where the source code is relative to the current directory

VPATH = ../../../Source/VandV_Calcs

# Definition of the object variables

objf = VandV_Calcs.o Functions.o ReadCommaDelimitedFile.o

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

# 64 bit
intel_win : FFLAGS = /O2 /Qsave /Qinit:zero /fp:strict /fp:except /traceback /stand:f08  $(GITINFO)
intel_win : FCOMPL = ifx
intel_win : obj = VandV_Calcs_win
intel_win : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

# ------------- LINUX ------------------

# gnu linux
gnu_linux : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_linux : FCOMPL = gfortran
gnu_linux : LFLAGS = 
gnu_linux : obj = VandV_Calcs_linux
gnu_linux : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 64 bit
intel_linux : FFLAGS = -m64 -O2 -save -zero -static -gen-interfaces -warn interfaces -traceback $(GITINFO)
intel_linux : LFLAGS = -static-intel
intel_linux : FCOMPL = ifx
intel_linux : obj = VandV_Calcs_linux
intel_linux : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

# ------------- OSX -------------------

# gnu osx
gnu_osx : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -ffpe-summary=none -fbacktrace $(GITINFOGNU)
gnu_osx : FCOMPL = gfortran
gnu_osx : LFLAGS = 
gnu_osx : obj = VandV_Calcs_osx
gnu_osx : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 64 bit
intel_osx : FFLAGS = -fpp -O2 -save -zero -m64 -heap-arrays -static-intel -fp-model strict $(GITINFO)
intel_osx : FCOMPL = ifort
intel_osx : obj = VandV_Calcs_osx
intel_osx : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


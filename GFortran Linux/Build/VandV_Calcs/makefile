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
intel_win_64 : FFLAGS = /O2 /Qsave /Qinit:zero /fp:strict /fp:except /traceback /stand:f08  $(GITINFO)
intel_win_64 : FCOMPL = ifort
intel_win_64 : obj = VandV_Calcs_win_64
intel_win_64 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

# ------------- LINUX ------------------

# gnu linux
gnu_linux_64 : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_linux_64 : FCOMPL = gfortran
gnu_linux_64 : LFLAGS = 
gnu_linux_64 : obj = VandV_Calcs_linux_64
gnu_linux_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 64 bit
intel_linux_64 : FFLAGS = -m64 -O2 -save -zero -static -gen-interfaces -warn interfaces -traceback $(GITINFO)
intel_linux_64 : LFLAGS = -static-intel
intel_linux_64 : FCOMPL = ifort
intel_linux_64 : obj = VandV_Calcs_linux_64
intel_linux_64 : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

# ------------- OSX -------------------

# gnu osx
gnu_osx_64 : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -ffpe-summary=none -fbacktrace $(GITINFOGNU)
gnu_osx_64 : FCOMPL = gfortran
gnu_osx_64 : LFLAGS = 
gnu_osx_64 : obj = VandV_Calcs_osx_64
gnu_osx_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 64 bit
intel_osx_64 : FFLAGS = -fpp -O2 -save -zero -m64 -heap-arrays -static-intel -fp-model strict $(GITINFO)
intel_osx_64 : FCOMPL = ifort
intel_osx_64 : obj = VandV_Calcs_osx_64
intel_osx_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


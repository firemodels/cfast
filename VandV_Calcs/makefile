# Makefile for VandV_Calcs

# To use this makefile, select the appropriate compiler and OS from the list below. For example, to
# compile the 32 bit version of CFAST under Linux using the Intel compilers type:
#
# make intel_linux_32

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

# The variable VPATH defines where the source code is relative to the current directory

VPATH = .

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

# gnu windows
gnu_win : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_win : FCOMPL = gfortran
gnu_win : LFLAGS = 
gnu_win : obj = VandV_Calcs_win_64
gnu_win : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# gnu windows debug
gnu_win_db : FFLAGS = -O0 -Og -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_win_db : FCOMPL = gfortran
gnu_win_db : LFLAGS = 
gnu_win_db : obj = VandV_Calcs_win_64_db
gnu_win_db : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 32 bit
intel_win_32_db : FFLAGS = /Qsave /Qinit:zero /debug:full /Z7 /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f95 $(GITINFO)
intel_win_32_db : FCOMPL = ifort
intel_win_32_db : obj = VandV_Calcs_win_32_db
intel_win_32_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

intel_win_32 : FFLAGS = /O2 /Qsave /Qinit:zero /fp:strict /fp:except /traceback /stand:f08  $(GITINFO)
intel_win_32 : FCOMPL = ifort
intel_win_32 : obj = VandV_Calcs_win_32
intel_win_32 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

# 64 bit
intel_win_64 : FFLAGS = /O2 /Qsave /Qinit:zero /fp:strict /fp:except /traceback /stand:f08  $(GITINFO)
intel_win_64 : FCOMPL = ifort
intel_win_64 : obj = VandV_Calcs_win_64
intel_win_64 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

intel_win_64_db : FFLAGS = /Qsave /Qinit:zero /debug:full /Z7 /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f08 $(GITINFO)
intel_win_64_db : FCOMPL = ifort
intel_win_64_db : obj = VandV_Calcs_win_64_db
intel_win_64_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin)  	

# ------------- Linux -------------------

# 32 bit
intel_linux_32 : FFLAGS = -fpp -m32 -O2 -save -zero -fp-model strict $(GITINFO)
intel_linux_32 : FCOMPL = ifort
intel_linux_32 : obj = VandV_Calcs_linux_32
intel_linux_32 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

intel_linux_32_db : FFLAGS = -m32 -check all -ftrapuv -warn unused -gen-interfaces -warn interfaces -O0 -save -zero -WB -traceback -g -fpe0 -fp-model strict -debug full $(GITINFO)
intel_linux_32_db : FCOMPL = ifort
intel_linux_32_db : obj = VandV_Calcs_linux_32_db
intel_linux_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# gnu linux
gnu_linux_64 : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_linux_64 : FCOMPL = gfortran
gnu_linux_64 : LFLAGS = 
gnu_linux_64 : obj = VandV_Calcs_linux_64
gnu_linux_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# gnu linux debug
gnu_linux_db : FFLAGS = -O0 -Og -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_linux_db : FCOMPL = gfortran
gnu_linux_db : LFLAGS = 
gnu_linux_db : obj = VandV_Calcs_linux_64_db
gnu_linux_db : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# gnu osx
gnu_osx_64 : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_osx_64 : FCOMPL = gfortran
gnu_osx_64 : LFLAGS = 
gnu_osx_64 : obj = VandV_Calcs_osx_64
gnu_osx_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 64 bit
intel_linux_64 : FFLAGS = -m64 -O2 -save -zero -static -gen-interfaces -warn interfaces -traceback $(GITINFO)
intel_linux_64 : LFLAGS = -static-intel
intel_linux_64 : FCOMPL = ifort
intel_linux_64 : obj = VandV_Calcs_linux_64
intel_linux_64 : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

#intel_linux_64_db : FFLAGS = -m64 -check all -ftrapuv -warn unused -gen-interfaces -warn interfaces -O0 -WB -traceback -g -fpe0 -fp-model strict -debug full
intel_linux_64_db : FFLAGS = -m64 -check -warn unused -O0 -save -zero -static -WB -traceback -g -fpe0 -fp-model strict -stand:f08 $(GITINFO)
intel_linux_64_db : LFLAGS = -static-intel
intel_linux_64_db : FCOMPL = ifort
intel_linux_64_db : obj = VandV_Calcs_linux_64_db
intel_linux_64_db : $(objf)
	$(FCOMPL) $(LFLAGS) -o $(obj) $(objf)

# ------------- OSX -------------------

# gnu osx
gnu_osx : FFLAGS = -O2 -finit-local-zero -ffpe-trap=invalid,zero,overflow -ffpe-summary=none -fbacktrace $(GITINFOGNU)
gnu_osx : FCOMPL = gfortran
gnu_osx : LFLAGS = 
gnu_osx : obj = VandV_Calcs_osx_64
gnu_osx : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# gnu osx_db
gnu_osx_db : FFLAGS = -O0 -Og -finit-local-zero -ffpe-trap=invalid,zero,overflow -fbacktrace $(GITINFOGNU)
gnu_osx_db : FCOMPL = gfortran
gnu_osx_db : LFLAGS = 
gnu_osx_db : obj = VandV_Calcs_osx_64_db
gnu_osx_db : $(objf)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 32 bit
intel_osx_32 : FFLAGS = -fpp -O2 -save -zero  -m32 -heap-arrays -static-intel $(GITINFO)
intel_osx_32 : FCOMPL = ifort
intel_osx_32 : obj = VandV_Calcs_osx_32
intel_osx_32 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_32_db : FFLAGS = -check -warn unused -O0 -save -zero -m32 -g -traceback -fpe:0 -fp-model strict -FR -WB $(GITINFO)
intel_osx_32_db : FCOMPL = ifort
intel_osx_32_db : obj = VandV_Calcs_osx_32_db
intel_osx_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

# 64 bit
intel_osx_64 : FFLAGS = -fpp -O2 -save -zero -m64 -heap-arrays -static-intel -fp-model strict $(GITINFO)
intel_osx_64 : FCOMPL = ifort
intel_osx_64 : obj = VandV_Calcs_osx_64
intel_osx_64 : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_64_db : FFLAGS = -fpp -check -warn unused -O0 -save -zero -m64 -g -traceback -save-temps -fpe:0 -fp-model strict -fp-stack-check -FR -WB $(GITINFO)
intel_osx_64_db : FCOMPL = ifort
intel_osx_64_db : obj = VandV_Calcs_osx_64_db
intel_osx_64_db : $(objf)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)
#*** End Compiler Specific Rules ***

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


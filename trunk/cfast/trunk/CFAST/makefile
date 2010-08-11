# Makefile for CFAST, Version 6

# To use this makefile, select the appropriate compiler and OS from the list below. For example, to
# compile the 32 bit version of CFAST under Linux using the Intel compilers type:
#
# make intel_linux_32

# A make utility for windows may be downloaded from http://gnuwin32.sourceforge.net/packages/make.htm

# The variable VPATH defines where the source code is relative to the current directory

VPATH = Source

# Definition of the object variables

objf = cfast.o auxiliary.o initialization.o input.o numerics.o \
              output.o outputsmv.o outputspreadsheet.o ssheaders.o \
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

.SUFFIXES: .c .f90 .f .o .obj

.f.o:
	$(FCOMPL) -c $(FFLAGS) $<
.f.obj:
	$(FCOMPL) -c $(FFLAGS) $<
.f90.o:
	$(FCOMPL) -c $(FFLAGS) $<
.f90.obj:
	$(FCOMPL) -c $(FFLAGS) $<
.c.o:
	$(CCOMPL) -c $(CFLAGS) $<
.c.obj:
	$(CCOMPL) -c $(CFLAGS) $<

# Windows Intel Compiler, version 11

 #*** note: the following scripts (located in ..\Scripts directory) must be run before running make with this target
 #    ..\Scripts\iclvars ia32
 #    ..\Scripts\ifortvars ia32

intel_win_32_db : FFLAGS = -fpp -D pp_ibmpc /Qvec_report0 /nologo /debug:full /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f95
intel_win_32_db : CFLAGS = -Dpp_noappend
intel_win_32_db : FCOMPL = ifort
intel_win_32_db : CCOMPL = cl
intel_win_32_db : obj = cfast6_win_32_db
intel_win_32_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

intel_win_32 : FFLAGS = -O2 -fpp -D pp_ibmpc /Qvec_report0
intel_win_32 : CFLAGS = -O2 -Dpp_noappend
intel_win_32 : FCOMPL = ifort
intel_win_32 : CCOMPL = cl
intel_win_32 : obj = cfast6_win_32
intel_win_32 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin)

intel_win_32all : FFLAGS = -O2 -fpp -D pp_ibmpc /Qvec_report0
intel_win_32all : CFLAGS = -O2 -Dpp_noappend
intel_win_32all : FCOMPL = ifort
intel_win_32all : CCOMPL = cl
intel_win_32all : obj = cfast6_win_32
intel_win_32all : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS)  /F100000000 $(objwin) 	

 #*** note: the following scripts (located in ..\Scripts directory) must be run before running make with this target
 #    ..\Scripts\iclvars intel64
 #    ..\Scripts\ifortvars intel64

intel_win_64 : FFLAGS = -O2 -fpp -D pp_ibmpc /Qvec_report0
intel_win_64 : CFLAGS = -O2 -Dpp_noappend
intel_win_64 : FCOMPL = ifort
intel_win_64 : CCOMPL = cl
intel_win_64 : obj = cfast6_win_64
intel_win_64 : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin) 

intel_win_64_db : FFLAGS = -fpp -D pp_ibmpc /Qvec_report0 /nologo /debug:full /Od /extend_source:132 /warn:all /warn:nointerfaces /Qtrapuv /fp:strict /fp:except /traceback /check:all /stand:f95
intel_win_64_db : CFLAGS = -O2 -Dpp_noappend
intel_win_64_db : FCOMPL = ifort
intel_win_64_db : CCOMPL = cl
intel_win_64_db : obj = cfast6_win_64_db
intel_win_64_db : setup_win $(objwin)
	$(FCOMPL) -o $(obj) $(FFLAGS) /F1000000000 $(objwin)  	

# Intel Compiler for Linux, version 11

intel_linux_32 : FFLAGS = -m32 -O3 -static -vec_report0
intel_linux_32 : CFLAGS = -m32 -O -Dpp_noappend -vec_report0
intel_linux_32 : FCOMPL = ifort
intel_linux_32 : CCOMPL = icc
intel_linux_32 : obj = cfast6_intel_linux_32
intel_linux_32 : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

intel_linux_64 : FFLAGS = -m64 -O3 -static -vec_report0
intel_linux_64 : CFLAGS = -m64 -O -Dpp_noappend -vec_report0
intel_linux_64 : FCOMPL = ifort
intel_linux_64 : CCOMPL = icc
intel_linux_64 : obj = cfast6_intel_linux_64
intel_linux_64 : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

intel_linux_32_db : FFLAGS = -m32 -check all -warn unused -O0 -auto -WB -traceback -g -fpe0 -fltconsistency -static -vec_report0 -ftrapuv -debug full
intel_linux_32_db : CFLAGS = -m32 -Dpp_noappend -vec_report0
intel_linux_32_db : FCOMPL = ifort
intel_linux_32_db : CCOMPL = icc
intel_linux_32_db : obj = cfast6_intel_linux_32_db
intel_linux_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

intel_linux_64_db : FFLAGS = -m64 -check -warn unused -O0 -auto -WB -traceback -g -fpe0 -fltconsistency -static -vec_report0
intel_linux_64_db : CFLAGS = -m64 -Dpp_noappend -vec_report0
intel_linux_64_db : FCOMPL = ifort
intel_linux_64_db : CCOMPL = icc
intel_linux_64_db : obj = cfast6_intel_linux_64_db
intel_linux_64_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

# 32 bit
intel_osx_32 : FFLAGS = -O3  -m32 -axSSSE3 -heap-arrays -static-intel -vec_report0
intel_osx_32 : CFLAGS = -O3 -m32 -Dpp_noappend -Dpp_OSX -vec_report0
intel_osx_32 : FCOMPL = ifort
intel_osx_32 : CCOMPL = icc
intel_osx_32 : obj = cfast6_intel_osx_32
intel_osx_32 : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_32_db : FFLAGS = -check -warn unused -O0 -m32 -g -traceback -fpe:0 -fltconsistency -FR -WB -vec_report0
intel_osx_32_db : CFLAGS = -O0 -m32 -g -traceback -Dpp_noappend -Dpp_OSX -vec_report0
intel_osx_32_db : FCOMPL = ifort
intel_osx_32_db : CCOMPL = icc
intel_osx_32_db : obj = cfast6_intel_osx_32_db
intel_osx_32_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

# 64 bit
intel_osx_64 : FFLAGS = -O3 -m64 -heap-arrays -axSSSE3 -static-intel
intel_osx_64 : CFLAGS = -O3 -m64 -Dpp_noappend -Dpp_OSX -vec_report0
intel_osx_64 : FCOMPL = ifort
intel_osx_64 : CCOMPL = icc
intel_osx_64 : obj = cfast6_intel_osx_64
intel_osx_64 : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

intel_osx_64_db : FFLAGS = -check -warn unused -O0 -m64 -g -traceback -save-temps -fpe:0 -fltconsistency -fp-stack-check -FR -WB -vec_report0
intel_osx_64_db : CFLAGS = -O0 -g -traceback -m64 -Dpp_noappend -Dpp_OSX -vec_report0
intel_osx_64_db : FCOMPL = ifort
intel_osx_64_db : CCOMPL = icc
intel_osx_64_db : obj = cfast6_intel_osx_64_db
intel_osx_64_db : setup $(obj)
	$(FCOMPL) $(FFLAGS) -mmacosx-version-min=10.4 -o $(obj) $(objf)

#*** IBM XLF Compiler ***

ibm_osx_ppc : FFLAGS = -O4 -qstrict -qextname -qarch=auto -qtune=auto
ibm_osx_ppc : CFLAGS = -O3 -D pp_OSX
ibm_osx_ppc : FCOMPL = /opt/ibmcmp/xlf/8.1/bin/xlf95
ibm_osx_ppc : CCOMPL = gcc
ibm_osx_ppc : obj = cfast6_osx_ppc_ibm
ibm_osx_ppc : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

#*** Lahey Compiler ***

lahey_linux : FFLAGS = -O --tpp --staticlink --nap --nchk --npca --nsav --ntrace
lahey_linux : CFLAGS = -O
lahey_linux : FCOMPL = lf95
lahey_linux : CCOMPL = gcc
lahey_linux : obj = cfast6_lahey
lahey_linux : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

lahey_linux_db : FFLAGS = --swm 3909 --wo --tpp --chk --f95 -g --warn --staticlink
lahey_linux_db : CFLAGS = -O
lahey_linux_db : FCOMPL = lf95
lahey_linux_db : CCOMPL = gcc
lahey_linux_db : obj = cfast6_lahey_db
lahey_linux_db : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

#*** Sun Solaris 10 f95 Compiler ***

f95_solaris10 : FFLAGS   = -O2 -m64 -Bstatic
f95_solaris10 : CFLAGS   = -O2 -m64
f95_solaris10 : FCOMPL   = f95
f95_solaris10 : CCOMPL   = cc
f95_solaris10 : obj = cfast6_solaris
f95_solaris10 : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

#*** Sun Solaris (SunOS 5.11) f95 Compiler - AMD64 Opteron(tm) Processor *** 

f95_solaris11 : FFLAGS   = -xarch=sse2a -m64 -xchip=native -Bstatic 
f95_solaris11 : CFLAGS   = -xarch=sse2a -m64 -xchip=native 
f95_solaris11 : FCOMPL   = f95 
f95_solaris11 : CCOMPL   = cc 
f95_solaris11 : obj = cfast6_solaris11 
f95_solaris11 : $(obj) 
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf) 

#*** Portland Group Compiler***

# AMD Opteron 64Bit PC
pgi_linux_64 : FFLAGS = -fast -tp k8-64 -Mstandard
pgi_linux_64 : CFLAGS = -fast -tp k8-64
pgi_linux_64 : FCOMPL = pgf90
pgi_linux_64 : CCOMPL = pgcc
pgi_linux_64 : LFLAGS  = -fast -tp k8-64 -byteswapio -Bstatic -lc -lgcc_eh
pgi_linux_64 : obj = cfast6_pgi_Opteron64
pgi_linux_64 : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)


#XLF-Compiler, IBM Power6 Core, OpenMP, full optimized settings
xlf_openmp : FFLAGS  = -O3 -qhot -q64 -qtune=pwr6 -qarch=pwr6 -qmaxmem=-1 -bdatapsize:64K -bstackpsize:64K -btextpsize:64K -qsmp=omp
xlf_openmp : CFLAGS  = -O3 -qhot -Dpp_noappend -q64 -qtune=pwr6 -qarch=pwr6 -qmaxmem=-1 -bdatapsize:64K -bstackpsize:64K -btextpsize:64K -qsmp=omp
xlf_openmp : FCOMPL  = xlf90_r
xlf_openmp : CCOMPL  = xlc_r
xlf_openmp : obj     = cfast6_xlf_openmp
xlf_openmp : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf) 

#XLF-Compiler, IBM Power6 Core, OpenMP, non-optimized setting
xlf_openmp_nooptimization : FFLAGS  = -O0 -q64 -qsmp=omp
xlf_openmp_nooptimization : CFLAGS  = -O0 -Dpp_noappend -q64 -qsmp=omp
xlf_openmp_nooptimization : FCOMPL  = xlf90_r
xlf_openmp_nooptimization : CCOMPL  = xlc_r
xlf_openmp_nooptimization : obj     = cfast6_xlf_openmp_nooptimization
xlf_openmp_nooptimization : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf) 

#*** GNU Compilers ***

#GCC and gfortran Version

gnu_win_32 : FFLAGS  = -O1 -ffree-line-length-none -ffree-form
gnu_win_32 : CFLAGS  = -O1
gnu_win_32 : FCOMPL  = gfortran
gnu_win_32 : CCOMPL  = gcc
gnu_win_32 : obj     = cfast6_gnu_win_32
gnu_win_32 : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

gnu_linux_32 : FFLAGS  = -O1 -ffree-line-length-none -ffree-form
gnu_linux_32 : CFLAGS  = -O1
gnu_linux_32 : FCOMPL  = gfortran
gnu_linux_32 : CCOMPL  = gcc
gnu_linux_32 : obj     = cfast6_gnu_linux_32
gnu_linux_32 : $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

gnu_linux_64 : FFLAGS = -O3 -unroll -static -frecord-marker=4
gnu_linux_64 : CFLAGS = -O3 -unroll -static
gnu_linux_64 : FCOMPL = gfortran
gnu_linux_64 : CCOMPL = gcc
gnu_linux_64 : obj = cfast6_gnu_linux_64
gnu_linux_64 : setup $(obj)
	$(FCOMPL) $(FFLAGS) -o $(obj) $(objf)

#*** End Compiler Specific Rules ***

# *** Object Dependencies ***


cfast.o : auxiliary.o initialization.o input.o numerics.o \
              output.o outputsmv.o outputspreadsheet.o ssheaders.o \
              conduction.o cyl_conduct.o convection.o radiation.o target.o \
              fire.o flowfan.o flowhall.o flowhorizontal.o flowvertical.o


cfast.obj : auxiliary.obj initialization.obj input.obj numerics.obj \
              output.obj outputsmv.obj outputspreadsheet.obj ssheaders.obj \
              conduction.obj cyl_conduct.obj convection.obj radiation.obj target.obj \
              fire.obj flowfan.obj flowhall.obj flowhorizontal.obj flowvertical.obj

#*** Clean Target to remove Object and Module files ***

.PHONY : clean
clean:
	-rm -f *.o *.mod *.obj 

.PHONY : winclean
winclean:
	-erase *.mod *.obj 


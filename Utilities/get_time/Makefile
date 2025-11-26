SOURCE_DIR = ..\source
INC_DIR =  -I $(SOURCE_DIR)
FILTERC =
FILTERF =

VPATH = $(SOURCE_DIR)
bin = .

# Definition of the object variables

obj =    get_time.o 
objwin = $(obj:.o=.obj)

# main has to be compiled without optimization (on a Windows PC) otherwise errors occcur
main.obj: CFLAGS = -Od /nologo -D WIN32

#*** General Purpose Rules ***

no_target:
	@echo \******** You did not specify a make target \********
	@echo Use one of targets found in the Makefile

.SUFFIXES: .F90 .c .obj .o .f90 .cpp

.c.obj:
	$(CC) -c $(CFLAGS) $(FILTERC) $(INC_DIR) $<
.c.o:
	$(CC) -c $(CFLAGS) $(FILTERC) $(INC2_DIR) $<
.F90.o:
	$(FC) -c $(FFLAGS) $(FILTERF) $<
.f90.obj:
	$(FC) -c $(FFLAGS) $(FILTERF) $<
.f90.o:
	$(FC) -c $(FFLAGS) $(FILTERF) $<



# ********  rules for each platform supported *************

intel_win : CFLAGS    = -Od /nologo -D WIN32 -D X64
intel_win : CC        = icl
intel_win : CPP       = icl
intel_win : FC        = ifort
intel_win : exe       = get_time.exe

intel_win : $(objwin)
	$(CPP) -o $(bin)/$(exe) $(objwin) 

LINUXFORTLIBS=$(FORTLIBDIR)/intel64/libifcore.a

# ------------- Linux 64 ----------------
#  64 bit linux

intel_linux : FFLAGS    = -O2 -m64 -fpp -D pp_noappend -D pp_LINUX64
intel_linux : CFLAGS    = -O2 -m64 -D pp_LINUX64
intel_linux : CC        = icc
intel_linux : CPP       = icc
intel_linux : FC        = ifort
intel_linux : exe       = smokezip_linux

intel_linux : $(obj)
	$(CPP) -o $(bin)/$(exe) -static-intel $(obj) $(LINUXFORTLIBS) -L $(LIB_DIR)/lib_linux_intel -lz -lpthread
	
OSXFORTLIBS=$(FORTLIBDIR)/libifcoremt.a $(FORTLIBDIR)/libimf.a $(FORTLIBDIR)/libirc.a $(FORTLIBDIR)/libsvml.a

# ------------- OSX 64 ----------------
#  64 bit OSX

intel_osx : FFLAGS    = -O2 -m64 -fpp -D pp_OSX64
intel_osx : CFLAGS    = -O2 -m64 -D pp_OSX64
intel_osx : CC        = icc
intel_osx : CPP       = icc
intel_osx : FC        = ifort
intel_osx : exe       = smokezip_osx

intel_osx : $(obj)
	$(CPP) -o $(bin)/$(exe) -m64 -static-intel $(obj)  -mmacosx-version-min=10.4 $(OSXFORTLIBS) -L $(LIB_DIR)/lib_osx_intel -lz -lpthread

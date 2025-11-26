SOURCE_DIR = src
INC_DIR = -I $(SOURCE_DIR)
FILTERC =
FILTERF =

VPATH = $(SOURCE_DIR)
bin = .
ifeq ($(shell echo "check_quotes"),"check_quotes")
  GIT_HASH := $(shell ..\..\..\Utilities\Scripts\githash)
  GIT_DATE := $(shell ..\..\..\Utilities\Scripts\gitlog)
else
  GIT_HASH := $(shell git describe --long --dirty)
  GIT_DATE := $(shell git log -1 --format=%cd)
endif
GITINFO=-Dpp_GITHASH=\"$(GIT_HASH)\" -Dpp_GITDATE=\""$(GIT_DATE)\""


# Definition of the object variables

obj = main.o string_util.o dmalloc.o
objwin = $(obj:.o=.obj)

#*** General Purpose Rules ***

no_target:
	@echo \******** You did not specify a make target \********
	@echo Use one of targets found in the Makefile

.SUFFIXES: .F90 .c .obj .o .f90 .cpp

.c.obj:
	$(CC) -c $(CFLAGS) $(FILTERC) $(INC_DIR) $<
.c.o:
	$(CC) -c $(CFLAGS) $(FILTERC) $(INC_DIR) $<

# ********  rules for each platform supported *************

# ------------- Win 64 ----------------
#  64 bit windows

intel_win : CFLAGS    = -O1 /nologo -D WIN32 -I $(SOURCE_DIR)/shared $(GITINFO)
intel_win : CC        = icl
intel_win : exe       = background.exe

intel_win : $(objwin)
	$(CC) -o $(bin)/$(exe) $(objwin) 

#-------------- compile sring_util.c so revision strings are updated -----------

string_util.o:  .FORCE
string_util.obj:        .FORCE

.FORCE:

